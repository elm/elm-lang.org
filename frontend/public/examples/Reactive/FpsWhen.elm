
-- The fpsWhen function gives deltas between each update.
-- So summing all of the deltas should give the time
-- that the signal has been running. In this case, it only
-- runs when the signal it is given is true. So this will
-- only update when the mouse is down.

import Graphics.Element (..)
import Mouse
import Signal
import Text (asText)
import Time (fpsWhen)


main : Signal Element
main =
  Signal.map asText (Signal.foldp (+) 0 (30 `fpsWhen` Mouse.isDown))