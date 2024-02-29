{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module DhallTypes
  ( Rule (..),
    RuleName (..),
    Grouping (..),
    Filtering (..),
    FilteringF (..),
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

import Data.Fix (Fix (..))
import qualified Data.Functor.Foldable.TH as TH
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
  | GroupTemporally SourceTemporal GroupingBucketTemporal GroupSelectionTemporal
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
  = AfterTemporal Int SortingOrder SourceTemporal
  | BeforeTemporal Int SortingOrder SourceTemporal
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GroupSelectionTemporal

data GroupingBucketTemporal
  = Daily
  | Weekly
  | Monthly
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GroupingBucketTemporal

data Filtering
  = AllF
  | AndF Filtering Filtering
  | OrF Filtering Filtering
  | GtFTemporal SourceTemporal SourceTemporal
  | LtFTemporal SourceTemporal SourceTemporal
  deriving stock (Eq, Show)

TH.makeBaseFunctor ''Filtering

deriving stock instance Generic (FilteringF a)

deriving anyclass instance FromDhall a => FromDhall (FilteringF a)

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    grouping :: Grouping,
    filtering :: Fix FilteringF,
    actions :: [Action]
  }
  deriving stock (Generic)

deriving anyclass instance FromDhall Rule
