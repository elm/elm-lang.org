
-- Click to stamp a pentagon at the current position.


mouseTuple = lift2 (\clkd pos -> (clkd,pos)) Mouse.isClicked Mouse.position

clickLocations =
  let f mt acc = case mt of { (clkd,pos) -> if clkd then pos:acc else acc } in
  foldp f [] mouseTuple

scene (w,h) = let clearBlue = rgba 0 (1/3) (2/3) (1/2) in
              collage w h . List.map (filled clearBlue . ngon 5 20)

main = lift2 scene Window.dimensions clickLocations
