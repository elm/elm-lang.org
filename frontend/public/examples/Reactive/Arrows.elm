import Graphics.Element exposing (..)
import Keyboard


main : Signal Element
main =
  Signal.map show Keyboard.arrows