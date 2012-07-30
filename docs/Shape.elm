
import Website.Docs (createDocs)

creation =
  [ ("ngon", "Number -> Number -> (Number,Number) -> Shape", "Create regular polygons with n sides. This function takes the number of sides, the radius (distance from the center to a vertex), and a center.")
  , ("rect", "Number -> Number -> (Number,Number) -> Shape", "Create a rectangle with a given width, height, and center position (in that order).")
  , ("oval", "Number -> Number -> (Number,Number) -> Shape", "Create an oval with a given width, height, and center position (in that order).")
  , ("polygon", "[(Number,Number)] -> (Number,Number) -> Shape", "Create an arbitrary polygon from an list of vertexes and a center point. The vertexes should be listed relative to the origin (0,0). They are then translated to the given center.")
  ]

toForm =
  [ ("filled", "Color -> Shape -> Form", "Fill a shape with a given color.")
  , ("outlined", "Color -> Shape -> Form", "Outline a shape with a given color.")
  , ("customOutline", "[Number] -> Color -> Shape -> Form", "Outline a shape with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  ]

transform =
  [ ("move", "Number -> Number -> Shape -> Shape", "Translate a shape by a given x and y offset.")
  , ("rotate", "Number -> Shape -> Shape", "Rotate a shape by a given fraction of a full turn (not degrees or radians). For instance, (rotate (1/2) s) will rotate s by a half turn.")
  , ("scale", "Number -> Shape -> Shape", "Scale a shape by a given scale factor.")
  ]

categories = 
  [ ("Creating Shapes", creation)
  , ("Transforming Shapes", transform)
  , ("Turning Shapes into Forms", toForm)
  ]

main = createDocs "Shape" categories