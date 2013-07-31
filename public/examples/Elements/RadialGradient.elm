
grad1 = radial (0,0) 50 (0,10) 90
          [(0  , rgb  244 242 1),
           (0.8, rgb  228 199 0),
           (1  , rgba 228 199 0 0)]

grad2 = radial (0,0) 15 (7,-5) 40
          [(0  , rgb  0 201 255),
           (0.8, rgb  0 181 226),
           (1  , rgba 0 181 226 0)]

grad3 = radial (0,0) 20 (7,-15) 50
          [(0   , rgb  255 95 152),
           (0.75, rgb  255 1 136),
           (1   , rgba 255 1 136 0)]

grad4 = radial (0,0) 10 (7,-5) 30
          [(0  , rgb  167 211 12),
           (0.9, rgb  1 159 98),
           (1  , rgba 1 159 98 0)]

main =
  collage 300 300 [
    circle 100 |> gradient grad1 |> move (-55,-55),
    circle 100 |> gradient grad2 |> move ( 40, 85),
    circle 100 |> gradient grad3 |> move ( 50,-10),
    circle 100 |> gradient grad4 |> move (-10, 50) ]
