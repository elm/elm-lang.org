import Graphics.Element exposing (..)
import Mouse


main : Signal Element
main =
  Signal.map show countClick


countClick : Signal Int
countClick =
  Signal.foldp (\clk count -> count + 1) 0 Mouse.clicks