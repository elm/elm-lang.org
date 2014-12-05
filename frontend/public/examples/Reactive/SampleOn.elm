
-- Displays the position of the latest click.

import Graphics.Element (..)
import Mouse
import Signal
import Text (asText)


main : Signal Element
main =
  Signal.map asText (Signal.sampleOn Mouse.clicks Mouse.position)