
clickLocations = foldp (::) [] (sampleOn Mouse.clicks Mouse.position)

scene (w,h) locs =
  let clearBlue = rgba 0 85 170 (1/2) in
  layers [ collage w h $ map (filled clearBlue . ngon 5 20) locs
         , plainText "Click to stamp a pentagon." ]

main = lift2 scene Window.dimensions clickLocations
