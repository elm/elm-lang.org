import Graphics.Element exposing (Element, show)
import Keyboard


main : Signal Element
main =
  Signal.map show Keyboard.wasd