
quadrant spc n =
    let xs = map (\x -> (x * spc, 0)) [0..n]
        ys = map (\y -> (0, y * spc)) <| reverse [0..n]
    in  zipWith (\x y -> line [x,y]) xs ys

main = collage 300 300 . map (solid black) <| quadrant 10 30