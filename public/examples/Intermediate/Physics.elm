
---- Prose explanation ----

intro = [markdown|
# Physics: How does it work?

Today we are going to learn how to gravity.
|]

body = [markdown|
Now that we can gravity, lets see if we can do it with elasticity!
|]

outro = [markdown|
Now you know how to gravity with elasticity! Good work physics friend!
|]


---- Figures ----

drawFig1 t =
  let pos = (150 + 120 * cos t, 150 + 80 * sin t) in
  collage 300 300 [ filled cyan $ circle 20 pos
                  , toForm pos (plainText "Earth")
                  , filled yellow $ circle 35 (150,150)
                  , toForm (150,150) (plainText "Sun") ]

figure1 = lift drawFig1 (Time.every 0.1)


drawFig2 t =
  let pos = (150, 250 - abs (200 * sin t)) in
  collage 300 300 [ filled red $ circle 15 pos
                  , filled green $ rect 300 50 (150,275) ]

figure2 = lift drawFig2 (Time.every 0.1)


---- Put it together and show it ----

presentation fig1 fig2 =
  flow down [ intro, fig1, body, fig2, outro ]

main = lift2 presentation figure1 figure2