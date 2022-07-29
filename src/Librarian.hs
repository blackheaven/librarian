module Librarian
  ( Rule (..),
    RuleName (..),

    -- * Collecting
    Matcher (..),
    Mover (..),
    CollectedFiles,
    fetchRulesOn,

    -- * Planning
    Move (..),
    planMoves,
    displayPlan,

    -- * Runner
    Action (..),
    ActionResult (..),
    RunResult,
    runPlan,
    displayResult,
  )
where

import Control.Exception (catch)
import Control.Monad
import Data.Function (on)
import Data.Functor (($>))
import Data.List (find, nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import GHC.Exts (IsString)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.EasyFile (splitFileName)
import System.FilePath.Glob (compile, globDir)
import Text.RegexPR
import Text.Show.Pretty

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    movers :: [Mover]
  }
  deriving stock (Eq, Show, Generic)

newtype RuleName = RuleName String
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)

newtype Matcher = Matcher {matchPattern :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)

data Mover = Mover
  { inputPattern :: String,
    newName :: String
  }
  deriving stock (Eq, Show, Generic)

type CollectedFiles = Map.Map FilePath Rule

fetchRulesOn :: FilePath -> [Rule] -> IO CollectedFiles
fetchRulesOn root rules = do
  matches <- globDir (compile . matchPattern . match <$> rules) root
  let distributeRule :: [FilePath] -> Rule -> [(FilePath, Rule)]
      distributeRule fs r = map (\f -> (f, r)) fs
  files <- mapM (filterM doesFileExist) matches
  return $ Map.unions $ map Map.fromList $ zipWith distributeRule files rules

data Move = Move
  { original :: FilePath,
    new :: Maybe FilePath,
    rule :: Rule
  }
  deriving stock (Eq, Show, Generic)

planMoves :: CollectedFiles -> [Move]
planMoves = map (uncurry planMove) . Map.toList
  where
    planMove :: FilePath -> Rule -> Move
    planMove p rule =
      Move
        { original = p,
          new = newPath p $ movers rule,
          rule = rule
        }
    newPath :: FilePath -> [Mover] -> Maybe FilePath
    newPath p =
      fmap (\mover -> subRegexPR (inputPattern mover) (newName mover) p)
        . find (isJust . flip matchRegexPR p . inputPattern)

displayPlan :: [Move] -> IO ()
displayPlan xs = do
  forM_ xs $ \Move {..} ->
    putStrLn $ "[" <> show (name rule) <> "] '" <> original <> "' -> '" <> fromMaybe "UNABLE TO REPLACE" new <> "'"
  let unrenamed = nubBy ((==) `on` name) $ sortOn name $ map rule $ filter (isJust . new) xs
  unless (null unrenamed) $ do
    putStrLn "Unable to generate a new name for:"
    forM_ unrenamed pPrint

data Action = Action
  { from :: FilePath,
    to :: FilePath
  }
  deriving stock (Eq, Show, Generic)

data ActionResult
  = Done
  | Existing
  | IOException IOError
  deriving stock (Eq, Show, Generic)

type RunResult = [(Action, ActionResult)]

runPlan :: [Move] -> IO RunResult
runPlan = fmap catMaybes . run
  where
    run =
      mapM $ \Move {..} ->
        case new of
          Nothing -> return Nothing
          Just newPath -> do
            let action = Action original newPath
            existing <- doesFileExist newPath
            if existing
              then return $ Just (action, Existing)
              else do
                prepareDirectory newPath
                Just . (,) action <$> ((renameFile original newPath $> Done) `catch` (return . IOException))
    prepareDirectory = createDirectoryIfMissing True . fst . splitFileName

displayResult :: RunResult -> IO ()
displayResult =
  mapM_ $ \(Action {..}, result) ->
    case result of
      Done -> return ()
      Existing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' ALREADY EXISTING"
      IOException e -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' IOError (" <> show e <> ")"
