{-# LANGUAGE OverloadedRecordDot #-}

module Convert (convert) where

import Data.Foldable (Foldable (foldl'))
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

convertFiltering :: S.Filtering -> T.Filtering
convertFiltering =
  \case
    S.All -> T.AllF
    S.Ands xs -> foldl' T.AndF T.AllF $ map convertFilteringCondition xs
    S.Ors xs -> foldl' T.OrF T.AllF $ map convertFilteringCondition xs
  where
    convertFilteringCondition =
      \case
        S.GtTemporalF x y -> T.GtF (convertSourceTemporal x) (convertSourceTemporal y)
        S.LtTemporalF x y -> T.LtF (convertSourceTemporal x) (convertSourceTemporal y)

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
