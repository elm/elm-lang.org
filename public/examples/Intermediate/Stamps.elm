
import Mouse
import Window

main = lift2 scene Window.dimensions clickLocations

-- for a good time, remove "sampleOn Mouse.clicks" ;)
clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

scene (w,h) locs =
  let drawPentagon (x,y) =
          ngon 5 20 |> filled (hsva (toFloat x) 1 1 0.7)
                    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
                    |> rotate (toFloat x)
  in  layers [ collage w h (map drawPentagon locs)
             , plainText "Click to stamp a pentagon." ]
