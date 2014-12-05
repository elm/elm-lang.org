import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List


main : Element
main =
  collage 300 300 (List.concatMap makeLevel [1..6])


makeLevel : Float -> List Form
makeLevel level =
  let n = 6 * level
      pointAt i = makePoint (level * 14) (turns (i/n))
  in
      List.map pointAt [0..n]


makePoint : Float -> Float -> Form
makePoint radius angle =
  circle 5
    |> filled (hsl (degrees radius) 0.8 0.6)
    |> move (radius * cos angle, radius * sin angle)
