
time = fst <~ timestamp (fps 40)

intro = [markdown|
# Physics: How does it work?

Today we are going to learn how to gravity.
|]

sunAndEarthAt time =
  let earth = group [ filled cyan (circle 20), toForm (plainText "Earth") ]
      sun = group [ filled yellow (circle 35), toForm (plainText "Sun") ]
      position = (120 * cos (inSeconds time), 80 * sin (inSeconds time))
  in  collage 300 300
        [ move position earth, move (25,0) sun ]

sunAndEarth = lift sunAndEarthAt time

body = [markdown|
Now that we can gravity, let's see if we can do it with elasticity!
|]

bouncingBallAt t =
  let y = abs (200 * sin (inSeconds t)) - 100
  in  collage 300 300
          [ move (0,y) (filled red (circle 15)),
            move (0,0-125) (filled green (rect 300 50)) ]

bouncingBall = lift bouncingBallAt time

outro = [markdown|
Now you know how to gravity with elasticity! Good work physics friend!
|]

---- Put it together and show it ----

presentation figure1 figure2 =
  flow down [ intro, figure1, body, figure2, outro ]

main = lift2 presentation sunAndEarth bouncingBall