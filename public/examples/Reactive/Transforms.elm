
import Mouse
import Window

scene (x,y) (w,h) =
  collage w h
    [ ngon 4 100 |> filled (rgb 0 85 170)
                 |> rotate (degrees x),
      ngon 5 30  |> filled (rgba 28 267 85 0.5)
                 |> move (x - toFloat w / 2, toFloat h / 2 - y)
    ]

main = lift2 scene Mouse.position Window.dimensions
