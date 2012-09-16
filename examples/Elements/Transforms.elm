
hexagon = ngon 6 40 (100,100)

main = collage 300 300
         [ filled blue hexagon
         , rotate 0.25 $ outlined red   hexagon
         , scale 2     $ outlined green hexagon
         , move 100 0  $ outlined black hexagon
         ]