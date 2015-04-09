
-- The fps function gives deltas between each update.
-- So summing all of the deltas should give the time
-- that the signal has been running:

import Graphics.Element exposing (..)
import Time exposing (fps)


main : Signal Element
main =
  Signal.map show (Signal.foldp (+) 0 (fps 30))