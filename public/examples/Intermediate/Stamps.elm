
import Mouse
import Window

clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

scene (w,h) locs =
  let clearBlue = rgba 0 85 170 (1/2)
      drawPentagon (x,y) = move x y . filled clearBlue $ ngon 5 20
  in layers [ collage w h (map drawPentagon locs)
            , plainText "Click to stamp a pentagon." ]

main = lift2 scene Window.dimensions clickLocations