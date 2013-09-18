
main = clock <~ every second

clock t = collage 400 400 [ filled black (ngon 12 110)
                          , hand red  100  t
                          , hand grey 100 (t/60)
                          , hand grey 60  (t/720) ]

hand clr len time =
  let angle = degrees (90 - 6 * inSeconds time)
  in  traced (solid clr) <| segment (0,0) (len * cos angle, len * sin angle)