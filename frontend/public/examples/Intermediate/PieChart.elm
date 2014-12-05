import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List (..)
import Mouse
import Signal
import Text (plainText)
import Window


main : Signal Element
main =
  Signal.map2 view Mouse.position Window.dimensions


view : (Int,Int) -> (Int,Int) -> Element
view (x,y) (w,h) =
  pieChart (map toFloat [x,y,w,h])


pieChart : List Float -> Element
pieChart numbers =
  let fracs = normalize numbers
      offsets = scanl (+) 0 fracs
  in
      collage 300 300 <|
      concat (map3 (pieSlice 100) colors offsets fracs) ++ [ filled white (circle 70) ]


pieSlice : Float -> Color -> Float -> Float -> List Form
pieSlice radius colr offset angle =
  let makePoint t = fromPolar (radius, degrees (360 * offset + t))
  in
      [ filled colr <| polygon ((0,0) :: map makePoint[ 0 .. 360 * angle ])
      , toForm (asPercent angle)
          |> move (fromPolar (radius*1.25, turns (offset + angle/2)))
      ]


asPercent : Float -> Element
asPercent fraction =
  plainText <| toString (toFloat (truncate (fraction * 100))) ++ "%"


colors : List Color
colors =
  [ lightBlue, lightGreen, lightYellow, lightRed
  , lightPurple, blue, green, yellow, red, purple
  ]


normalize : List Float -> List Float
normalize xs =
  let total = sum xs
  in
      map (\x -> x/total) xs
