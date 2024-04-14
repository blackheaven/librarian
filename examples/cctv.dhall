let L = ../format.dhall

in  [ L.Rule::{
      , name = "All footages"
      , match = "CCTV/**/*.*m*"
      , actions =
        [ L.Action.Type.Move
            { inputPattern = "CCTV/(.*)/(\\d+)\\.mp4"
            , newName = "sorted/\\2/\\1.mp4"
            }
        ]
      }
    ]
