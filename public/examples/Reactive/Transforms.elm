
import Mouse
import Window

scene (x,y) (w,h) =
  collage w h
    [ ngon 4 100 |> filled (rgb 0 85 170)
                 |> rotate (degrees (toFloat x)),
      ngon 5 30  |> filled (rgba 28 267 85 0.5)
                 |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    ]

main = lift2 scene Mouse.position Window.dimensions
