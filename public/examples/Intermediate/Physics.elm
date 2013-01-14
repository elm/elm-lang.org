
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

time = timeOf (fps 40)

drawFig1 t =
  let pos = ( 150 + 120 * cos (inSeconds t)
            , 150 +  80 * sin (inSeconds t) )
  in  collage 300 300 [ filled cyan $ circle 20 pos
                      , toForm pos (plainText "Earth")
                      , filled yellow $ circle 35 (125,150)
                      , toForm (125,150) (plainText "Sun") ]

figure1 = lift drawFig1 time


drawFig2 t =
  let pos = (150, 250 - abs (200 * sin (inSeconds t))) in
  collage 300 300 [ filled red $ circle 15 pos
                  , filled green $ rect 300 50 (150,275) ]

figure2 = lift drawFig2 time


---- Put it together and show it ----

presentation fig1 fig2 =
  flow down [ intro, fig1, body, fig2, outro ]

main = lift2 presentation figure1 figure2