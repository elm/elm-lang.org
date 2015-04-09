import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


main : Element
main =
  collage 150 150 <| List.map shape [0..11]


shape : Int -> Form
shape n =
  let angle = degrees (30 * toFloat n)
  in
      circle 10
        |> filled (hsl angle 0.7 0.5)
        |> move (45 * cos angle, 45 * sin angle)

