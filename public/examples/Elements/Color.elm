
myBlue  = rgb 0 85 170
myGreen = rgba 28 267 85 (1/2)

main = collage 300 300 [ filled myBlue  $ ngon 4 75 (150, 150)
                       , filled myGreen $ ngon 5 50 (200, 100)
                       ]