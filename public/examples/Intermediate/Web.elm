
quadrant spc n =
    let xs = map (\x -> (x * spc, 0)) <| [0..n]
        ys = map (\y -> (0, y * spc)) <| reverse [0..n]
    in  map (traced (solid black)) (zipWith segment xs ys)

quad angle =
    rotate (degrees angle) . group <| quadrant 8 20

main = collage 300 300 <| map quad [ 0, 90, 180, 270 ]