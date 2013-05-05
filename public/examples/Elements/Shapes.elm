
main = collage 300 300
       [ rect 200 200 |> outlined (solid black)
       , oval 140 140 |> outlined (dashed blue)
       , ngon   5  60 |> filled green
       ]
