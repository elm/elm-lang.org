import Mouse
import Window

main = lift2 scene Mouse.position Window.dimensions

scene (x,y) (w,h) =
  let (dx,dy) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  in  collage w h
       [ ngon 3 100 |> filled blue
                    |> rotate (atan2 dy dx)
       , ngon 6 30  |> filled orange
                    |> move (dx, dy)
       ]