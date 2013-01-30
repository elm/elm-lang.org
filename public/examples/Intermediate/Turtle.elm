
-- Move the Turtle around with the arrow keys. Use the
-- space bar to make the turtle come above water for air.

obj = { x = 0, y = 0, a = 0, v = 0, surfaced = False }

spacestep spc obj = { obj | surfaced <- spc }
keystep d obj = { obj | v <- d.y
                      , a <- obj.a + 0.02 * d.x }
timestep t obj = let {x,y,a,v} = obj in
                 { obj | x <- x + t * v * cos a
                       , y <- y + t * v * sin a }

step (space,arrows,time) =
  timestep time . keystep arrows . spacestep space


render (w,h) obj =
  let trans = rotate (obj.a / (2*pi)) . move obj.x obj.y
      order = if obj.surfaced then reverse else id
  in layers $ order
       [ collage w h [ trans . toForm (100,100) $
                       image 96 96 "turtle.gif" ]
       , opacity 0.7 $ fittedImage w h "water.gif" ]


delta = lift (flip (/) 25) (fps 30)
input = sampleOn delta $
        lift3 (,,) Keyboard.space Keyboard.arrows delta

main  = lift2 render Window.dimensions (foldp step obj input)

-- Try switching out Keyboard.arrows for Keyboard.wasd to
-- try out different controls (or swich Keyboard.space for
-- Keyboard.ctrl or Keyboard.shift).