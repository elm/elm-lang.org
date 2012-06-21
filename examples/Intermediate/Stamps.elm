
-- Click to stamp a pentagon at the current position.

import Signal.Mouse (isClicked,position)
import Signal.Window (dimensions)

mouseTuple = lift2 (\clkd pos -> (clkd,pos)) isClicked position

clickLocations =
  let f mt acc = case mt of { (clkd,pos) -> if clkd then pos:acc else acc } in
  foldp f [] mouseTuple

scene (w,h) = let clearBlue = rgba 0 85 170 (1/2) in
              collage w h . List.map (filled clearBlue . ngon 5 20)

main = lift2 scene dimensions clickLocations
