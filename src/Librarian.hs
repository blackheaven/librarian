{-# OPTIONS_GHC -Wno-partial-fields #-}

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
    planActions,
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
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (IsString)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.EasyFile (splitFileName)
import System.FilePath.Glob (compile, globDir)
import Text.RegexPR

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

data Action
  = Move {inputPattern :: String, newName :: String}
  | Copy {inputPattern :: String, newName :: String}
  | Remove {inputPattern :: String}
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

data ResolvedAction
  = ResolvedMove {original :: FilePath, new :: FilePath, rule :: Rule}
  | ResolvedCopy {original :: FilePath, new :: FilePath, rule :: Rule}
  | ResolvedRemove {original :: FilePath, rule :: Rule}
  deriving stock (Eq, Show, Generic)

planActions :: CollectedFiles -> [ResolvedAction]
planActions = concatMap (take 1 . uncurry planAction) . Map.toList
  where
    planAction :: FilePath -> Rule -> [ResolvedAction]
    planAction p rule = mapMaybe go $ actions rule
      where
        go :: Action -> Maybe ResolvedAction
        go =
          \case
            Move {..} ->
              newPath inputPattern newName
                <&> \newPath' -> ResolvedMove {original = p, new = newPath', rule = rule}
            Copy {..} ->
              newPath inputPattern newName
                <&> \newPath' -> ResolvedCopy {original = p, new = newPath', rule = rule}
            Remove {..} ->
              matchRegexPR inputPattern p
                $> ResolvedRemove {original = p, rule = rule}
        newPath :: String -> String -> Maybe FilePath
        newPath inputPattern' newName' =
          mfilter (/= p) $ Just $ subRegexPR inputPattern' newName' p

displayPlan :: [ResolvedAction] -> IO ()
displayPlan =
  mapM_ $ \case
    ResolvedMove {..} ->
      putStrLn $ "Move [" <> getRuleName (name rule) <> "] '" <> original <> "' -> '" <> new <> "'"
    ResolvedCopy {..} ->
      putStrLn $ "Copy [" <> getRuleName (name rule) <> "] '" <> original <> "' -> '" <> new <> "'"
    ResolvedRemove {..} ->
      putStrLn $ "Remove [" <> getRuleName (name rule) <> "] '" <> original <> "'"

data FsAction
  = FsMove {from :: FilePath, to :: FilePath}
  | FsCopy {from :: FilePath, to :: FilePath}
  | FsRemove {from :: FilePath}
  deriving stock (Eq, Show, Generic)

data ActionResult
  = Done
  | Existing
  | Missing
  | IOException IOError
  deriving stock (Eq, Show, Generic)

type RunResult = [(FsAction, ActionResult)]

runPlan :: [ResolvedAction] -> IO RunResult
runPlan = fmap catMaybes . run
  where
    run =
      mapM $
        \case
          ResolvedMove {..} -> do
            let fsAction = FsMove original new
            originalPresent <- doesFileExist original
            if not originalPresent
              then return $ Just (fsAction, Missing)
              else do
                newPresent <- doesFileExist new
                if newPresent
                  then return $ Just (fsAction, Existing)
                  else do
                    prepareDirectory new
                    Just . (,) fsAction <$> ((renameFile original new $> Done) `catch` (return . IOException))
          ResolvedCopy {..} -> do
            let fsAction = FsCopy original new
            originalPresent <- doesFileExist original
            if not originalPresent
              then return $ Just (fsAction, Missing)
              else do
                newPresent <- doesFileExist new
                if newPresent
                  then return $ Just (fsAction, Existing)
                  else do
                    prepareDirectory new
                    Just . (,) fsAction <$> ((copyFile original new $> Done) `catch` (return . IOException))
          ResolvedRemove {..} -> do
            let fsAction = FsRemove original
            originalPresent <- doesFileExist original
            if not originalPresent
              then return $ Just (fsAction, Missing)
              else Just . (,) fsAction <$> ((removeFile original $> Done) `catch` (return . IOException))
    prepareDirectory = createDirectoryIfMissing True . fst . splitFileName

displayResult :: RunResult -> IO ()
displayResult =
  mapM_ $ \(action, result) ->
    case action of
      FsMove {..} ->
        case result of
          Done -> return ()
          Existing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' ALREADY EXISTING"
          Missing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' MISSING"
          IOException e -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' IOError (" <> show e <> ")"
      FsCopy {..} ->
        case result of
          Done -> return ()
          Existing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' ALREADY EXISTING"
          Missing -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' MISSING"
          IOException e -> putStrLn $ "'" <> from <> "' -> '" <> to <> "' IOError (" <> show e <> ")"
      FsRemove {..} ->
        case result of
          Done -> return ()
          Existing -> putStrLn $ "'" <> from <> "' ALREADY EXISTING (BUG)"
          Missing -> putStrLn $ "'" <> from <> "' MISSING"
          IOException e -> putStrLn $ "'" <> from <> "' IOError (" <> show e <> ")"
