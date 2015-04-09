
-- Displays the position of the latest click.

import Graphics.Element exposing (..)
import Mouse


main : Signal Element
main =
  Signal.map show (Signal.sampleOn Mouse.clicks Mouse.position)