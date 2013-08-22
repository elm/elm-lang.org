
main = draw <~ ball

-- Draw the sky, ball, and ground
draw ball =
  collage 150 400
   [ rect 150 400 |> filled (rgb 135 206 250)
   , circle 15    |> filled red
                  |> move (0, ball.height - 160)
   , rect 150 50  |> filled green
                  |> move (0,-200)
   ]

-- Physics
gravity = 140
bounceVelocity = 250

step time ball =
  { ball | height   <- ball.height + ball.velocity * time,
           velocity <- if | ball.height < 0 -> bounceVelocity
                          | otherwise -> ball.velocity - gravity * time }

ball = foldp step {height=0, velocity=bounceVelocity}
                  (inSeconds <~ fps 30)
