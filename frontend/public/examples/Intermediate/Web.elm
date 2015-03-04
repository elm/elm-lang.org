import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)


main : Element
main =
  collage 300 300 (map quad [ 0, 90, 180, 270 ])


quad : Float -> Form
quad angle =
  quadrant 8 20
    |> group
    |> rotate (degrees angle)


quadrant : Float -> Int -> List Form
quadrant spc n =
  let scale a = toFloat a * spc
      xs = map (\x -> (scale x, 0)) <| [0..n]
      ys = map (\y -> (0, scale y)) <| reverse [0..n]
  in
      map (traced (solid black)) (map2 segment xs ys)
