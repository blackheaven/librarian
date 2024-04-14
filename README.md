# Librarian

Move/rename according a set of rules.

## Example

With `example/cctv.dhall`:

```dhall
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
```

```
$ librarian -r examples/cctv.dhall
[All footages] './CCTV/garage/220520.mp4' -> './sorted/220520/garage.mp4'
[All footages] './CCTV/garage/220521.mp4' -> './sorted/220521/garage.mp4'
[All footages] './CCTV/garage/220522.mp4' -> './sorted/220522/garage.mp4'
[All footages] './CCTV/garage/220523.mp4' -> './sorted/220523/garage.mp4'
[All footages] './CCTV/garden/220520.mp4' -> './sorted/220520/garden.mp4'
[All footages] './CCTV/garden/220521.mp4' -> './sorted/220521/garden.mp4'
[All footages] './CCTV/garden/220522.mp4' -> './sorted/220522/garden.mp4'
[All footages] './CCTV/garden/220523.mp4' -> './sorted/220523/garden.mp4'
Move? (y/n)
```
