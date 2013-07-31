
grad = linear (0,60) (0,-60)
         [(0, rgb 0 171 235),
          (0.79, white),
          (0.8, rgb 38 192 0),
          (1, white)]

main =
  collage 120 120
    [ gradient grad (rect 120 120),
      toForm (image 35 35 "/imgs/mario/jump/right.gif") ]
