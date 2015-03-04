import Graphics.Element exposing (..)
import Mouse


main : Varying Element
main =
  Varying.map show countClick


countClick : Varying Int
countClick =
  Stream.fold (\clk count -> count + 1) 0 Mouse.clicks