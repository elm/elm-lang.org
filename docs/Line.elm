
import Website.Docs (createDocs)

creation =
  [ ("line", "[(Number,Number)] -> Line", "Create an arbitrary line from an list of vertexes.")
  ]

toForm =
  [ ("solid", "Color -> Line -> Form", "Create a solid line with a given color.")
  , ("dashed", "Color -> Line -> Form", "Create a dashed line with a given color.")
  , ("dotted", "Color -> Line -> Form", "Create a dotted line with a given color.")
  , ("customLine", "[Number] -> Color -> Line -> Form", "Create a custom line with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  ]

transform =
  [ ("move", "Number -> Number -> Line -> Line", "Translate a shape by a given x and y offset.")
  , ("rotate", "Number -> Number -> Line -> Line", "Rotate a shape by a given fraction of a full turn (not degrees or radians). For instance, (rotate (1/2) s) will rotate s by a half turn.")
  , ("scale", "Number -> Line -> Line", "Scale a shape by a given scale factor.")
  ]

categories = 
  [ ("Creating Lines", creation)
  , ("Transforming Lines", transform)
  , ("Turning Lines into Forms", toForm)
  ]

main = createDocs "Line" categories