[
  { name = "All footages"
  , match = "CCTV/**/*.*m*"
  , movers = [
        { inputPattern = "CCTV/(.*)/(\\d+)\\.mp4"
        , newName = "sorted/\\2/\\1.mp4"
        }
    ]
  }
]
