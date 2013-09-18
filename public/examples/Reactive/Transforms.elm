import Mouse
import Window

main = lift2 scene Mouse.position Window.dimensions

scene (x,y) (w,h) =
  let dx = (toFloat x - toFloat w / 2)
      dy = (toFloat h / 2 - toFloat y)
  in  collage w h
       [ ngon 3 100
          |> filled blue
          |> rotate (atan2 dy dx)
       , ngon 6 30
          |> filled red
          |> move (dx, dy)
       ]