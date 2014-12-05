import Graphics.Element (..)
import Signal (Signal, map)
import Mouse
import Text (asText)


main : Signal Element
main =
  map asText Mouse.position