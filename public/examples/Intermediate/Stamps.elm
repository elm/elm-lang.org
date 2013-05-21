
import Mouse
import Window

clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

scene (w,h) locs =
  let drawPentagon (x,y) = ngon 5 20 |> filled (rgba 0 85 170 0.5)
                                     |> move (x- toFloat w/2, toFloat h/2-y)
  in layers [ collage w h (map drawPentagon locs)
            , plainText "Click to stamp a pentagon." ]

main = lift2 scene Window.dimensions clickLocations