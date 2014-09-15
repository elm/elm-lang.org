
quadrant : Float -> Int -> [Form]
quadrant spc n =
    let scale a = toFloat a * spc
        xs = map (\x -> (scale x, 0)) <| [0..n]
        ys = map (\y -> (0, scale y)) <| reverse [0..n]
    in
        map (traced (solid black)) (zipWith segment xs ys)

quad : Float -> Form
quad angle =
    quadrant 8 20
        |> group
        |> rotate (degrees angle) 

main : Element
main =
    collage 300 300 (map quad [ 0, 90, 180, 270 ])