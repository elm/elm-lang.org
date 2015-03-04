
-- The fps function gives deltas between each update.
-- So summing all of the deltas should give the time
-- that the signal has been running:

import Graphics.Element exposing (..)
import Time


main : Varying Element
main =
  Varying.map show (Stream.fold (+) 0 (Time.fps 30))