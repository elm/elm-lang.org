
shape n =
  let angle = degrees (30 * toFloat n)
      color = hsv (30*n) 1 1
  in  circle 10 |> filled color
                |> move (45 * cos angle) (45 * sin angle)

main = collage 150 150 <| map shape [0..11]