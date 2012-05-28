
niceBlue   = rgb  ( 0 ) (1/3) (2/3)
clearGreen = rgba (1/9) (8/9) (3/9) (1/2)

main = collage 300 300 [ filled niceBlue   $ ngon 4 75 (150, 150)
                       , filled clearGreen $ ngon 5 50 (200, 100)
                       ]