
---- Prose explanation ----

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


---- Figures ----

time = fst <~ timestamp (fps 40)

drawFig1 t =
  let x = 120 * cos (inSeconds t)
      y =  80 * sin (inSeconds t)
  in  collage 300 300 [ move x y . filled cyan <| circle 20
                      , move x y . toForm <| plainText "Earth"
                      , move 25 0 . filled yellow <| circle 35
                      , move 25 0 . toForm <| plainText "Sun"
                      ]

figure1 = lift drawFig1 time


drawFig2 t =
  let y = abs (200 * sin (inSeconds t)) - 100
  in  collage 300 300 [ move 0 y . filled red $ circle 15
                      , move 0 (0-125) . filled green $ rect 300 50 ]

figure2 = lift drawFig2 time


---- Put it together and show it ----

presentation fig1 fig2 =
  flow down [ intro, fig1, body, fig2, outro ]

main = lift2 presentation figure1 figure2