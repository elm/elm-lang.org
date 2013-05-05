
myBlue  = rgb 0 85 170
myGreen = rgba 28 267 85 0.5

main = collage 300 300
       [ ngon 4 75 |> filled myBlue,
         ngon 5 50 |> filled myGreen |> move 50 10 ]