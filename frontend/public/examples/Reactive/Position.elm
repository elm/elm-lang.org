import Graphics.Element exposing (..)
import Signal exposing (Signal, map)
import Mouse
import Text exposing (asText)


main : Signal Element
main =
  map asText Mouse.position