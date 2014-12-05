import Graphics.Element (..)
import Keyboard
import Signal (Signal, map)
import Text (asText)


main : Signal Element
main =
  map asText Keyboard.arrows