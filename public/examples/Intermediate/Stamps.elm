
-- Click to stamp a pentagon at the current position.

import Mouse (clicks,position)
import Window (dimensions)

clickLocations = foldp (:) [] (sampleOn clicks position)

scene (w,h) locs =
  let clearBlue = rgba 0 85 170 (1/2) in
  layers [ collage w h $ map (filled clearBlue . ngon 5 20) locs
         , plainText "Click to stamp a pentagon." ]

main = lift2 scene dimensions clickLocations
