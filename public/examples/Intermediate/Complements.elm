
colorCycle t =
  let at t   = move <| fromPolar (100, t)
      toDot r t = at t . filled (hsv t 0.9 0.9) <| circle r
      t1 = t / 2000
      t2 = t / 2000 + degrees 180
  in  collage 300 300 <|
        toDot 20 t1 :: 
        toDot 20 t2 :: map (\angle -> toDot 10 (turns <| angle / 12)) [0..11]

main = lift colorCycle (foldp (+) 0 (fps 30))
