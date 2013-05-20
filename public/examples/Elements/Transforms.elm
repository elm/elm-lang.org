
hexagon = ngon 6 40

bordered clr = outlined (solid clr)

main = collage 300 300
         [ hexagon |> bordered red
         , hexagon |> bordered blue   |> rotate (degrees 30)
         , hexagon |> bordered green  |> scale 2
         , hexagon |> bordered purple |> move (100,0)
         ]