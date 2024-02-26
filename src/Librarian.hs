module Librarian
  ( Rule (..),
    RuleName (..),

    -- * Collecting
    Matcher (..),
    Action (..),
    CollectedFiles,
    fetchRulesOn,

    -- * Planning
    ResolvedAction (..),
    planMoves,
    displayPlan,

    -- * Runner
    FsAction (..),
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
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.String (IsString)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.EasyFile (splitFileName)
import System.FilePath.Glob (compile, globDir)
import Text.RegexPR
import Text.Show.Pretty

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    actions :: [Action]
  }
  deriving stock (Eq, Show, Generic)

instance FromDhall Rule

newtype RuleName = RuleName {getRuleName :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)
  deriving (FromDhall) via String

newtype Matcher = Matcher {matchPattern :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)
  deriving (FromDhall) via String

data Action = Move
  { inputPattern :: String,
    newName :: String
  }
  deriving stock (Eq, Show, Generic)

instance FromDhall Action

type CollectedFiles = Map.Map FilePath Rule

fetchRulesOn :: FilePath -> [Rule] -> IO CollectedFiles
fetchRulesOn root rules = do
  matches <- globDir (compile . matchPattern . match <$> rules) root
  let distributeRule :: [FilePath] -> Rule -> [(FilePath, Rule)]
      distributeRule fs r = map (\f -> (f, r)) fs
  files <- mapM (filterM doesFileExist) matches
  return $ Map.unions $ map Map.fromList $ zipWith distributeRule files rules

data ResolvedAction = ResolvedMove
  { original :: FilePath,
    new :: Maybe FilePath,
    rule :: Rule
  }
  deriving stock (Eq, Show, Generic)

planMoves :: CollectedFiles -> [ResolvedAction]
planMoves = map (uncurry planMove) . Map.toList
  where
    planMove :: FilePath -> Rule -> ResolvedAction
    planMove p rule =
      ResolvedMove
        { original = p,
          new = newPath p $ actions rule,
          rule = rule
        }
    newPath :: FilePath -> [Action] -> Maybe FilePath
    newPath p =
      fmap (\action -> subRegexPR (inputPattern action) (newName action) p)
        . find (isJust . flip matchRegexPR p . inputPattern)

displayPlan :: [ResolvedAction] -> IO ()
displayPlan xs = do
  forM_ xs $ \ResolvedMove {..} ->
    putStrLn $ "[" <> getRuleName (name rule) <> "] '" <> original <> "' -> '" <> fromMaybe "UNABLE TO REPLACE" new <> "'"
  let unrenamed = nubBy ((==) `on` name) $ sortOn name $ map rule $ filter (isNothing . new) xs
  unless (null unrenamed) $ do
    putStrLn "Unable to generate a new name for:"
    forM_ unrenamed pPrint

data FsAction = FsMove
  { from :: FilePath,
    to :: FilePath
  }
  deriving stock (Eq, Show, Generic)

data ActionResult
  = Done
  | Existing
  | IOException IOError
  deriving stock (Eq, Show, Generic)

type RunResult = [(FsAction, ActionResult)]

runPlan :: [ResolvedAction] -> IO RunResult
runPlan = fmap catMaybes . run
  where
    run =
      mapM $ \ResolvedMove {..} ->
        case new of
          Nothing -> return Nothing
          Just newPath -> do
            let fsAction = FsMove original newPath
            existing <- doesFileExist newPath
            if existing
              then return $ Just (fsAction, Existing)
              else do
                prepareDirectory newPath
                Just . (,) fsAction <$> ((renameFile original newPath $> Done) `catch` (return . IOException))
    prepareDirectory = createDirectoryIfMissing True . fst . splitFileName

displayResult :: RunResult -> IO ()
displayResult =
  mapM_ $ \(FsMove {..}, result) ->
    case result of
      Done -> return ()
      Existing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' ALREADY EXISTING"
      IOException e -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' IOError (" <> show e <> ")"
