 
main = lift colorCycle (foldp (+) 0 (fps 30))

colorCycle time =
  let toDot angle = circle (20 + 10 * sin (angle * 2 + inSeconds time))
                      |> filled (hsv angle 0.9 0.9)
                      |> move (fromPolar (100, angle + pi))
  in  collage 300 300 (map (\n -> toDot <| turns (n/12)) [0..11])
