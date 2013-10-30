
main = collage 150 150 <| map shape [0..11]

shape n =
  let angle = degrees (30 * toFloat n)
  in  circle 10 |> filled (hsv angle 0.7 1)
                |> move (45 * cos angle, 45 * sin angle)

