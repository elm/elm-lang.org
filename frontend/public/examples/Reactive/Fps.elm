
-- The fps function gives deltas between each update.
-- So summing all of the deltas should give the time
-- that the signal has been running:

import Graphics.Element exposing (..)
import Signal
import Text exposing (asText)
import Time exposing (fps)


main : Signal Element
main =
  Signal.map asText (Signal.foldp (+) 0 (fps 30))