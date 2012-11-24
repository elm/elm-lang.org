
import Website.Docs (createDocs)

creation =
    [ ("rgb", "Int -> Int -> Int -> Color", "Create rgb colors from numbers between 0 and 255 inclusive.")
    , ("rgba", "Int -> Int -> Int -> Float -> Color", "Create RGB colors with an alpha component for transparency. The alpha component is specified with numbers between 0 and 1.")
    , ("complement", "Color -> Color", "Produces a &ldquo;complementary color&rdquo;. The two colors will accent each other.")
    , ("hsv", "Int -> Float -> Float -> Color", "Create HSV colors. HSV stands for hue-saturation-value.\n\nHue is a degree from 0 to 360 representing a color wheel: red at 0&deg;, green at 120&deg;, blue at 240&deg;, and red again at 360&deg;. This makes it easy to cycle through colors and compute color complements, triads, tetrads, etc.\n\nSaturation is a number between 1 and 0 where lowering this number makes your color more grey. This can help you tone a color down.\n\nValue is also a number between 1 and 0. Lowering this number makes your color more black.\n\nLook up the &ldquo;HSV cylinder&rdquo; for more information.")
    , ("hsva", "Int -> Float -> Float -> Float -> Color", "Create HSV colors with an alpha component for transparency. The alpha component is specified with numbers between 0 and 1.")
    ]

builtin =
  [ ("red, green, blue, cyan, yellow, magenta, black, white, grey, gray", "Color", "Built-in colors.")
  ]


categories = 
  [ ("Built-in Colors", builtin)
  , ("Creating Colors", creation)
  ]

main = createDocs "Color" categories