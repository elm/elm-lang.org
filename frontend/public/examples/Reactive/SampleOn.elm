
-- Displays the position of the latest click.

import Graphics.Element exposing (..)
import Mouse


main : Varying Element
main =
  Varying.map show (Stream.sample always Mouse.position Mouse.clicks)