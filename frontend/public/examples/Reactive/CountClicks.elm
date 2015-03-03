import Graphics.Element exposing (..)
import Signal exposing (Signal, map, foldp)
import Mouse
import Text exposing (asText)


main : Signal Element
main =
  map asText countClick


countClick : Signal Int
countClick =
  foldp (\clk count -> count + 1) 0 Mouse.clicks