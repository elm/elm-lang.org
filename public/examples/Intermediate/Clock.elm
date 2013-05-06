
hand clr len time =
  let t = degrees (6 * inSeconds time - 90)
  in  traced (solid clr) <| segment (0,0) (len * cos t, len * sin t)

clock t = collage 400 400
  [ filled (rgb 96 176 224) (ngon 12 110)
  , hand  red  100 t
  , hand black 100 (t/60)
  , hand black 60  (t/720) ]

main = lift clock (every second)