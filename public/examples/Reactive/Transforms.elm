import Mouse
import Window

main = lift2 scene Mouse.position Window.dimensions

scene (x,y) (w,h) =
  let dx = (toFloat x - toFloat w / 2)
      dy = (toFloat h / 2 - toFloat y)
  in  collage w h
       [ ngon 3 100
          |> filled (rgb 0 85 170)
          |> rotate (atan2 dy dx)
       , ngon 6 30
          |> filled (rgba 255 127 0 0.7)
          |> move (dx, dy)
       ]