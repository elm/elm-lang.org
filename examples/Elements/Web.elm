
quadrant spcSize n =
    let xs = map (\x -> (x * spcSize, 0)) [0..n]
      , ys = map (\y -> (0, y * spcSize)) $ reverse [0..n]
    in  zipWith line xs ys

main = canvas 300 300 . map (solid black) $ quadrant 10 30