
makePoint radius angle =
    circle 5 |> filled (hsv (round radius) 1 1)
             |> move (radius * cos angle) (radius * sin angle)

makeLevel level =
  let n = 6 * level
      pointAt i = makePoint (level * 14) (turns (i/n))
  in  map pointAt [0..n]

main = collage 300 300 (concatMap makeLevel [1..6])