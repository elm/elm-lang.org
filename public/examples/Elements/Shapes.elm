
clearGrey = rgba 111 111 111 0.6

main = collage 300 300
       [ filled clearGrey (ngon 4 75),
         filled clearGrey (ngon 5 50) |> move (50,10) ]