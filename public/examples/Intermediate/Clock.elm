main = lift clock (every second)

radius = 100
shortHandLength = radius * 0.6
longHandLength = radius * 0.9

clock t = collage 400 400 [ filled lightGrey (circle radius)
                          , outlined (solid grey) (circle radius)
                          , hand orange longHandLength t
                          , hand charcoal longHandLength (t/60)
                          , hand charcoal shortHandLength (t/720) ]

hand clr len time =
  let angle = degrees (90 - 6 * inSeconds time)
  in traced (solid clr) <| segment (0,0) (len * cos angle, len * sin angle)
