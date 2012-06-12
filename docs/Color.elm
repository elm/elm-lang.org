
import Website.Docs (createDocs)

creation =
    [ ("rgb", "Number -> Number -> Number -> Color", "Create colors from numbers between 0 and 1.")
    , ("rgba", "Number -> Number -> Number -> Number -> Color", "Create colors with an alpha component for transparency, from numbers between 0 and 1.")
    , ("red, green, blue, black, white", "Color", "Built-in colors.")
    ]

categories = 
  [ ("Creating Colors", creation)
  ]

main = createDocs "Color" categories