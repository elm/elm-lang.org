import Graphics.Element exposing (..)
import Keyboard


main : Varying Element
main =
  Varying.map show Keyboard.wasd