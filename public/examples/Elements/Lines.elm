
outline = path [ (50,50), (50,-50), (-50,-50), (-50,50), (50,50) ]

main = collage 200 420
         [ outline |> traced (dashed blue)  |> move (0, -110)
         , outline |> traced (dotted green)
         , outline |> traced (solid  red)   |> move (0, 110)
         ]