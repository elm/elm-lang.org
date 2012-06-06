
quadrant spc n =
    let { xs = List.map (\x -> (x * spc, 0)) [0..n]
        ; ys = List.map (\y -> (0, y * spc)) $ List.reverse [0..n]
        }
    in  List.zipWith (\x y -> line [x,y]) xs ys

main = collage 300 300 . List.map (solid black) $ quadrant 10 30