
-- Click to stamp a pentagon at the current position.


mouseTuple = lift3 (\c x y -> (c,x,y)) Mouse.isClicked Mouse.x Mouse.y

clickLocations =
    let f mt acc = case mt of { (c,x,y) -> if c then (x,y):acc else acc } in
    foldp f [] mouseTuple

scene w h = let clearBlue = rgba 0 (1/3) (2/3) (1/2) in
            canvas w h . map (filled clearBlue . ngon 5 20)

main = lift3 scene Window.width Window.height clickLocations
