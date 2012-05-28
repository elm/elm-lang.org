
square   = rect 200 200 (150,150)
circle   = oval 140 140 (150,150)
pentagon = ngon   5  60 (150,150)

main = collage 300 300
         [ outlined black square
         , filled green pentagon
         , customOutline [8,4] blue circle
         ]