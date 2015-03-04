import Graphics.Element exposing (..)
import Mouse
import Text exposing (asText)


main : Varying Element
main =
  map asText Mouse.position