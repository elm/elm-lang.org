
hexagon = ngon 6 40 (100,100)

main = collage 300 300
         [ filled blue hexagon
         , outlined red   $ rotate (1/4) hexagon
         , outlined green $ scale 2 hexagon
         , outlined black $ move 100 0 hexagon
         ]