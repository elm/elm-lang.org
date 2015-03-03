
-- Displays the position of the latest click.

import Graphics.Element exposing (..)
import Mouse
import Signal
import Text exposing (asText)


main : Signal Element
main =
  Signal.map asText (Signal.sampleOn Mouse.clicks Mouse.position)