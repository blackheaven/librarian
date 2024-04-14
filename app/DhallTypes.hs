{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module DhallTypes
  ( Rule (..),
    RuleName (..),
    Grouping (..),
    Filtering (..),
    FilteringCondition (..),
    SourceTemporal (..),
    SourceDate (..),
    TimeSpec (..),
    SortingOrder (..),
    GroupSelectionTemporal (..),
    GroupingBucketTemporal (..),

    -- * Collecting
    Matcher (..),
    Action (..),
  )
where

import Data.String (IsString)
import Data.Time (UTCTime)
import Dhall

newtype RuleName = RuleName {getRuleName :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString, FromDhall)

newtype Matcher = Matcher {matchPattern :: String}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, IsString, FromDhall)

data Action
  = Move {inputPattern :: String, newName :: String}
  | Copy {inputPattern :: String, newName :: String}
  | Remove {inputPattern :: String}
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Action

data Grouping
  = FileGroup
  | GroupTemporally
      { sourceTemporal :: SourceTemporal,
        groupingBucketTemporal :: GroupingBucketTemporal,
        groupSelectionTemporal :: GroupSelectionTemporal
      }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Grouping

data SourceTemporal
  = SourceDate SourceDate
  | SourceTime TimeSpec
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SourceTemporal

data SourceDate
  = ModificationTime
  | AccessTime
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SourceDate

data TimeSpec
  = HoursAgo Integer
  | DaysAgo Integer
  | AbsoluteTime UTCTime
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall TimeSpec

data SortingOrder
  = SortingAsc
  | SortingDesc
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SortingOrder

data GroupSelectionTemporal
  = AfterTemporal {index :: Int, order :: SortingOrder, sourceTemporal :: SourceTemporal}
  | BeforeTemporal {index :: Int, order :: SortingOrder, sourceTemporal :: SourceTemporal}
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GroupSelectionTemporal

data GroupingBucketTemporal
  = Daily
  | Weekly
  | Monthly
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GroupingBucketTemporal

data FilteringCondition
  = GtTemporalF SourceTemporal SourceTemporal
  | LtTemporalF SourceTemporal SourceTemporal
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall FilteringCondition

data Filtering
  = All
  | Ands [FilteringCondition]
  | Ors [FilteringCondition]
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall Filtering

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    grouping :: Grouping,
    filtering :: Filtering,
    actions :: [Action]
  }
  deriving stock (Generic)

deriving anyclass instance FromDhall Rule
