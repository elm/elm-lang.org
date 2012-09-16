
-- Click to stamp a pentagon at the current position.

import Signal.Mouse (isClicked,position)
import Signal.Window (dimensions)

clickLocations = foldp (:) [] (sampleOn isClicked position)

scene (w,h) locs =
  let clearBlue = rgba 0 85 170 (1/2) in
  layers [ collage w h $ map (filled clearBlue . ngon 5 20) locs
         , plainText "Click to stamp a pentagon." ]

main = lift2 scene dimensions clickLocations
