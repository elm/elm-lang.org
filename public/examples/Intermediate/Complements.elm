
(box,checked) = Input.checkbox False

colorCycle t =
  let toPos t   = (150 + 100 * cos (pi*t/180), 150 + 100 * sin (pi*t/180))
      toDot r t = filled (hsv t 1 1) $ circle r (toPos t)
      t1 = (t / 100) `mod` 360
      t2 = (180 + t / 100) `mod` 360
  in  flow down [ collage 300 300 $
                          toDot 20 t1 :: 
                          toDot 20 t2 :: map (toDot 10 . (*) 30) [0..11]
                , container 300 40 middle $ plainText "On / Off  " `beside` box ]

main = lift colorCycle (foldp (+) 0 (30 `fpsWhen` checked))
