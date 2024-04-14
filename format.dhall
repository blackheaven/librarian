let SourceTime =
      { Type =
          < HoursAgo : Integer
          | DaysAgo : Integer
          | AbsoluteTime : { date : Date, time : Time, timeZone : TimeZone }
          >
      , default = {=}
      }

let SourceDate = { Type = < ModificationTime | AccessTime >, default = {=} }

let SourceTemporal =
      { Type = < SourceDate : SourceDate.Type | SourceTime : SourceTime.Type >
      , default = {=}
      }

let SortingOrder = { Type = < SortingAsc | SortingDesc >, default = {=} }

let GroupingBucketTemporal =
      { Type = < Daily | Weekly | Monthly >, default = {=} }

let Grouping =
      { Type =
          < FileGroup
          | GroupTemporally :
              { sourceTemporal : SourceTemporal.Type
              , groupingBucketTemporal : GroupingBucketTemporal.Type
              , groupSelectionTemporal :
                  < AfterTemporal :
                      { index : Integer
                      , order : SortingOrder.Type
                      , sourceTemporal : SourceTemporal.Type
                      }
                  | BeforeTemporal :
                      { index : Integer
                      , order : SortingOrder.Type
                      , sourceTemporal : SourceTemporal.Type
                      }
                  >
              }
          >
      , default = {=}
      }

let FilteringCondition =
      { Type =
          < GtTemporalF : { _1 : SourceTemporal.Type, _2 : SourceTemporal.Type }
          | LtTemporalF : { _1 : SourceTemporal.Type, _2 : SourceTemporal.Type }
          >
      , default = {=}
      }

let Filtering =
      { Type =
          < All
          | Ands : List FilteringCondition.Type
          | Ors : List FilteringCondition.Type
          >
      , default = {=}
      }

let Action =
      { Type =
          < Move : { inputPattern : Text, newName : Text }
          | Copy : { inputPattern : Text, newName : Text }
          | Remove : { inputPattern : Text }
          >
      , default = {=}
      }

let Rule =
      { Type =
          { name : Text
          , match : Text
          , grouping : Grouping.Type
          , filtering : Filtering.Type
          , actions : List Action.Type
          }
      , default =
        { grouping = Grouping.Type.FileGroup, filtering = Filtering.Type.All }
      }

in  { Rule
    , SourceTime
    , SourceDate
    , SourceTemporal
    , SortingOrder
    , GroupingBucketTemporal
    , Grouping
    , Filtering
    , Action
    }
