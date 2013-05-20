
main = lift clock (every second)

clock t =
    let blu = hsv (degrees 200) 0.6 1
    in  collage 400 400 [ filled blu (ngon 12 110)
                        , hand (complement blu) 100 t
                        , hand black 100 (t/60)
                        , hand black 60  (t/720) ]

hand clr len time =
  let angle = degrees (6 * inSeconds time - 90)
  in  traced (solid clr) <| segment (0,0) (len * cos angle, len * sin angle)