
import List (zipWith)

quadrant spc n =
    let { scale = map ((*) spc)
        ; xs = map (\x -> (x,0)) . scale $ [0..n]
        ; ys = map (\y -> (0,y)) . scale $ reverse [0..n]
    } in  zipWith segment xs ys

main = collage 300 300 . map (solid black) $ quadrant 10 30