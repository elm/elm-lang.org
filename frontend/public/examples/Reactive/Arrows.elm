import Graphics.Element exposing (..)
import Keyboard
import Signal exposing (Signal, map)
import Text exposing (asText)


main : Signal Element
main =
  map asText Keyboard.arrows