
-- Move the Turtle around with the arrow keys. Use the
-- space bar to make the turtle come above water for air.


-- MODEL
turtle = { x=0, y=0, a=0, v=0 }


-- UPDATE
keys d trtl =
    { trtl | v <- d.y, a <- trtl.a + 0.02 * d.x }
swim t trtl =
    let {x,y,a,v} = trtl in
    { trtl | x <- x + t * v * cos a,
             y <- y + t * v * sin a }

step (space,arrows,time) = swim time . keys arrows


-- DISPLAY
display (w,h) obj =
  let turtle  = image 96 96 "turtle.gif" |> toForm
                                         |> rotate (radians obj.a)
                                         |> move obj.x obj.y
  in layers [ collage w h [turtle],
              opacity 0.7 <| fittedImage w h "water.gif" ]


-- TURTLE
delta = lift (flip (/) 25) (fps 30)
input = sampleOn delta $
        lift3 (,,) Keyboard.space Keyboard.arrows delta

main  = lift2 display Window.dimensions (foldp step turtle input)

-- Try switching out Keyboard.arrows for Keyboard.wasd to
-- try out different controls (or swich Keyboard.space for
-- Keyboard.ctrl or Keyboard.shift).