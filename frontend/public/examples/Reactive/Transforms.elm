import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Mouse
import Signal (Signal, map2)
import Text (asText)
import Window


main : Signal Element
main =
  map2 scene Mouse.position Window.dimensions


scene : (Int,Int) -> (Int,Int) -> Element
scene (x,y) (w,h) =
  let (dx,dy) =
        (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  in
      collage w h
        [ ngon 3 100
            |> filled blue
            |> rotate (atan2 dy dx)
        , ngon 6 30
            |> filled orange
            |> move (dx, dy)
        ]