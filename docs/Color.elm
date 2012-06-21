
import Website.Docs (createDocs)

creation =
    [ ("rgb", "Int -> Int -> Int -> Color", "Create rgb colors from numbers between 0 and 255 inclusive.")
    , ("rgba", "Int -> Int -> Int -> Number -> Color", "Create colors with an alpha component for transparency. The color is specified with numbers between 0 and 255 inclusive, whereas the alpha component is specified with numbers between 0 and 1.")
    , ("red, green, blue, black, white, grey, gray, cyan, yellow, magenta", "Color", "Built-in colors.")
    ]

categories = 
  [ ("Creating Colors", creation)
  ]

main = createDocs "Color" categories