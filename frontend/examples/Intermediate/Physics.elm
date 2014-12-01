
---- Put it together and show it ----

main = lift2 presentation sunAndEarth bouncingBall

presentation figure1 figure2 =
  flow down [ intro, figure1, body, figure2, outro ]

---- Text ----

intro = [markdown|

# Physics: How does it work?

Today we are going to learn how to gravity.
|]

body = [markdown|
Now that we can gravity, let's see if we can do it with elasticity!
|]

outro = [markdown|
Now you know how to gravity with elasticity! Good work physics friend!
|]

---- Diagrams ----

time = lift (inSeconds << fst) (timestamp (fps 40))

sunAndEarthAt angle =
  let earth = group [ filled lightBlue (circle 20), toForm (plainText "Earth") ]
      sun = group [ filled lightYellow (circle 35), toForm (plainText "Sun") ]
  in  collage 300 200
        [ earth |> move (120 * cos angle, 80 * sin angle)
        , sun   |> move (25,0) ]

sunAndEarth = lift sunAndEarthAt time

bouncingBallAt angle =
  let ball = filled red (circle 15)
      ground = filled green (rect 300 50)
  in  collage 300 200
          [ ball   |> move (0, abs (150 * sin angle) - 75),
            ground |> move (0,-100) ]

bouncingBall = lift bouncingBallAt time
