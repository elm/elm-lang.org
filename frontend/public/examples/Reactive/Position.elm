import Graphics.Element exposing (..)
import Mouse


main : Signal Element
main =
  Signal.map show Mouse.position