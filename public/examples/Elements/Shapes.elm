
shape n =
  let angle = degrees (30 * toFloat n)
  in  circle 10 |> filled (hsv angle 1 1)
                |> move (45 * cos angle, 45 * sin angle)

main = collage 150 150 <| map shape [0..11]