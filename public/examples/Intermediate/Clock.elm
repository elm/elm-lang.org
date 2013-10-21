-- An 8-line analog clock.

main = clock <~ every second

clock t = collage 400 400 [ filled grey (ngon 12 110)
                          , hand orange 100  t
                          , hand  blue  100 (t/60)
                          , hand  blue  60  (t/720) ]

hand clr len time =
  let angle = degrees (90 - 6 * inSeconds time)
  in  traced (solid clr) <| segment (0,0) (len * cos angle, len * sin angle)