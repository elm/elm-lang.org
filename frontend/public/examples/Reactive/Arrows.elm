import Graphics.Element exposing (..)
import Keyboard
import Text exposing (asText)


main : Varying Element
main =
  map asText Keyboard.arrows