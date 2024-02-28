{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Librarian
  ( Rule (..),
    RuleName (..),
    Grouping (..),
    Filtering (..),
    Source (..),
    SourceDate (..),
    TimeSpec (..),
    SortingOrder (..),
    GroupSelection (..),
    GroupingBucket (..),

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
import Data.Foldable (Foldable (toList))
import Data.Functor (($>), (<&>))
import Data.Kind (Type)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Time
  ( UTCTime,
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    nominalDay,
    secondsToNominalDiffTime,
  )
import GHC.Generics (Generic)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getAccessTime,
    getModificationTime,
    removeFile,
    renameFile,
  )
import System.EasyFile (splitFileName)
import System.FilePath.Glob (compile, globDir)
import Text.RegexPR

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    grouping :: Grouping,
    filtering :: Filtering,
    actions :: [Action]
  }
  deriving stock (Show, Generic)

newtype RuleName = RuleName {getRuleName :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)

newtype Matcher = Matcher {matchPattern :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString)

data Action
  = Move {inputPattern :: String, newName :: String}
  | Copy {inputPattern :: String, newName :: String}
  | Remove {inputPattern :: String}
  deriving stock (Eq, Show, Generic)

data Grouping
  = FileGroup
  | forall a.
    Ord a =>
    Group
      { groupSource :: Source a,
        groupBucket :: GroupingBucket a,
        groupSelection :: GroupSelection a
      }

deriving stock instance Show Grouping

data Filtering
  = AllF
  | AndF Filtering Filtering
  | OrF Filtering Filtering
  | forall a. Ord a => GtF (Source a) (Source a)
  | forall a. Ord a => LtF (Source a) (Source a)

deriving stock instance Show Filtering

data Source :: Type -> Type where
  SourceDate :: SourceDate -> Source UTCTime
  SourceTime :: TimeSpec -> Source UTCTime

deriving stock instance Eq (Source a)

deriving stock instance Show (Source a)

data SourceDate
  = ModificationTime
  | AccessTime
  deriving stock (Eq, Show, Generic)

data TimeSpec
  = HoursAgo Integer
  | DaysAgo Integer
  | AbsoluteTime UTCTime
  deriving stock (Eq, Show, Generic)

data SortingOrder
  = SortingAsc
  | SortingDesc
  deriving stock (Eq, Show, Generic)

data GroupSelection a
  = After Int SortingOrder (Source a)
  | Before Int SortingOrder (Source a)
  deriving stock (Eq, Show, Generic)

data GroupingBucket :: Type -> Type where
  Daily :: GroupingBucket UTCTime
  Weekly :: GroupingBucket UTCTime
  Monthly :: GroupingBucket UTCTime

deriving stock instance Eq (GroupingBucket a)

deriving stock instance Show (GroupingBucket a)

type CollectedFiles = Map.Map FilePath (Seq Rule)

fetchRulesOn :: FilePath -> [Rule] -> IO CollectedFiles
fetchRulesOn root rules = do
  matches <- globDir (compile . matchPattern . match <$> rules) root
  let applyRule :: [FilePath] -> Rule -> IO [(FilePath, Seq Rule)]
      applyRule files rule =
        filterM (applyFiltering rule.filtering) files
          >>= applyGrouping rule
      applyFiltering :: Filtering -> FilePath -> IO Bool
      applyFiltering filteringRule file =
        case filteringRule of
          AllF -> return True
          AndF x y -> (&&) <$> applyFiltering x file <*> applyFiltering y file
          OrF x y -> (||) <$> applyFiltering x file <*> applyFiltering y file
          GtF x y -> (>) <$> fetchSource x file <*> fetchSource y file
          LtF x y -> (<) <$> fetchSource x file <*> fetchSource y file
      applyGrouping :: Rule -> [FilePath] -> IO [(FilePath, Seq Rule)]
      applyGrouping rule files =
        case rule.grouping of
          FileGroup ->
            return $ map (\f -> (f, [rule])) files
          Group {..} -> do
            let bucketUtc :: UTCTime -> String
                bucketUtc x =
                  case groupBucket of
                    Daily -> formatTime defaultTimeLocale "%Y-%m-%d" x
                    Weekly -> formatTime defaultTimeLocale "%Y-%V" x
                    Monthly -> formatTime defaultTimeLocale "%Y-%m" x
                bucket :: Source x -> x -> String
                bucket source x =
                  case source of
                    SourceDate _ -> bucketUtc x
                    SourceTime _ -> bucketUtc x
                fetchBucket :: Source x -> FilePath -> IO String
                fetchBucket source file =
                  bucket source <$> fetchSource source file
                sorting :: Ord x => SortingOrder -> [(x, FilePath)] -> [(x, FilePath)]
                sorting =
                  \case
                    SortingAsc -> sortOn fst
                    SortingDesc -> sortOn $ Down . fst
                applySelection :: [FilePath] -> IO [FilePath]
                applySelection bucketedFiles =
                  case groupSelection of
                    After n order source ->
                      drop (n + 1) . map snd . sorting order
                        <$> mapM (\file -> (,file) <$> fetchSource source file) bucketedFiles
                    Before n order source ->
                      take n . map snd . sorting order
                        <$> mapM (\file -> (,file) <$> fetchSource source file) bucketedFiles
            buckets <-
              Map.fromListWith @String @(Seq FilePath) (<>)
                <$> forM files (\file -> (,[file]) <$> fetchBucket groupSource file)
            concatMap (map (,[rule]))
              <$> mapM applySelection (toList <$> Map.elems buckets)
      fetchSource :: Source a -> FilePath -> IO a
      fetchSource source file =
        case source of
          SourceDate sourceDate ->
            case sourceDate of
              ModificationTime -> getModificationTime file
              AccessTime -> getAccessTime file
          SourceTime timeSpec ->
            case timeSpec of
              HoursAgo d ->
                addUTCTime (secondsToNominalDiffTime $ (-1) * fromInteger d * 60 * 60) <$> getCurrentTime
              DaysAgo d ->
                addUTCTime ((-1) * fromInteger d * nominalDay) <$> getCurrentTime
              AbsoluteTime x ->
                return x
  files <- mapM (filterM doesFileExist) matches
  Map.unionsWith (<>) . map Map.fromList <$> zipWithM applyRule files rules

data ResolvedAction
  = ResolvedMove {original :: FilePath, new :: FilePath, rule :: Rule}
  | ResolvedCopy {original :: FilePath, new :: FilePath, rule :: Rule}
  | ResolvedRemove {original :: FilePath, rule :: Rule}
  deriving stock (Show, Generic)

planActions :: CollectedFiles -> [ResolvedAction]
planActions = concatMap (take 1 . uncurry planAction) . concatMap (traverse toList) . Map.toList
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
