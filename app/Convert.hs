{-# LANGUAGE OverloadedRecordDot #-}

module Convert (convert) where

import qualified Data.Fix as Fix
import qualified Data.Functor.Foldable as Foldable
import Data.Time (UTCTime)
import qualified DhallTypes as S
import qualified Librarian as T

convert :: S.Rule -> T.Rule
convert x =
  T.Rule
    { name = T.RuleName x.name.getRuleName,
      match = T.Matcher x.match.matchPattern,
      grouping = convertGrouping x.grouping,
      filtering = convertFiltering x.filtering,
      actions = convertAction <$> x.actions
    }

convertGrouping :: S.Grouping -> T.Grouping
convertGrouping =
  \case
    S.FileGroup -> T.FileGroup
    S.GroupTemporally source bucket selection ->
      T.Group
        { groupSource = convertSourceTemporal source,
          groupBucket = convertGroupingBucketTemporal bucket,
          groupSelection = convertGroupSelectionTemporal selection
        }

convertFiltering :: Fix.Fix S.FilteringF -> T.Filtering
convertFiltering = go . Fix.foldFix Foldable.embed
  where
    go =
      \case
        S.AllF -> T.AllF
        S.AndF x y -> T.AndF (go x) (go y)
        S.OrF x y -> T.OrF (go x) (go y)
        S.GtFTemporal x y -> T.GtF (convertSourceTemporal x) (convertSourceTemporal y)
        S.LtFTemporal x y -> T.LtF (convertSourceTemporal x) (convertSourceTemporal y)

convertAction :: S.Action -> T.Action
convertAction =
  \case
    S.Move {..} -> T.Move {inputPattern = inputPattern, newName = newName}
    S.Copy {..} -> T.Copy {inputPattern = inputPattern, newName = newName}
    S.Remove {..} -> T.Remove {inputPattern = inputPattern}

convertSourceTemporal :: S.SourceTemporal -> T.Source UTCTime
convertSourceTemporal =
  \case
    S.SourceDate x -> T.SourceDate $ convertSourceDate x
    S.SourceTime x -> T.SourceTime $ convertTimeSpec x

convertGroupingBucketTemporal :: S.GroupingBucketTemporal -> T.GroupingBucket UTCTime
convertGroupingBucketTemporal =
  \case
    S.Daily -> T.Daily
    S.Weekly -> T.Weekly
    S.Monthly -> T.Monthly

convertGroupSelectionTemporal :: S.GroupSelectionTemporal -> T.GroupSelection UTCTime
convertGroupSelectionTemporal =
  \case
    S.AfterTemporal index sortingOrder source ->
      T.After index (convertSortingOrder sortingOrder) (convertSourceTemporal source)
    S.BeforeTemporal index sortingOrder source ->
      T.Before index (convertSortingOrder sortingOrder) (convertSourceTemporal source)
  where
    convertSortingOrder =
      \case
        S.SortingAsc -> T.SortingAsc
        S.SortingDesc -> T.SortingDesc

convertSourceDate :: S.SourceDate -> T.SourceDate
convertSourceDate =
  \case
    S.ModificationTime -> T.ModificationTime
    S.AccessTime -> T.AccessTime

convertTimeSpec :: S.TimeSpec -> T.TimeSpec
convertTimeSpec =
  \case
    S.HoursAgo x -> T.HoursAgo x
    S.DaysAgo x -> T.DaysAgo x
    S.AbsoluteTime x -> T.AbsoluteTime x

-- convertXX :: S.XX -> T.XX
-- convertXX =
--   \case {}
