
import Website.Docs

textContent =
    [ ("plainText", "String -> Element", "Display unstyled strings.")
    , ("text", "Text -> Element", "Display styled text.")
    , ("centeredText", "Text -> Element", "Display centered, styled text.")
    , ("rightedText", "Text -> Element", "Display right justified, styled text.")
    , ("justifiedText", "Text -> Element", "Display justified, styled text.")
    ]

visualContent =
    [ ("image", "Int -> Int -> String -> Element", "Display images based on dimensions and resource location.")
    , ("fittedImage", "Int -> Int -> String -> Element", "Display images fitted to a given width and height. The image will be scaled such that it fills the desired area without stretching the image. If the aspect ratio of the given image does not match, it will be cropped and centered.")
    , ("images", "Signal String -> Signal Element", "Asynchronously loads images given their resource locaition. The images dimensions are the default size of the image.")
    , ("video", "Int -> Int -> String -> Element", "Display videos based on dimensions and resource location.")
    , ("collage", "Int -> Int -> [Form] -> Element", "Renders the scene specified by the given width, height, and list of primitive graphical forms. These forms can be anything including lines, shapes, images, and Elements. Every Form can be moved, scaled, and rotated. More on forms later.")
    ]

combiningContent =
    [ ("flow", "Direction -> [Element] -> Element", "Display a list of Elements, flowing in the given direction.")
    , ("up, down, left, right, inward, outward", "Direction", "The six possible directions for content flow. Used only with the flow function.")
    , ("above", "Element -> Element -> Element", "(a `above` b) is the same as (flow down [a,b]), placing element a above element b")
    , ("below", "Element -> Element -> Element", "(a `below` b) is the same as (flow down [b,a]), placing element a below element b")
    , ("beside", "Element -> Element -> Element", "(a `beside` b) is the same as (flow right [a,b]), placing element a to the left of element b.")
    , ("layers", "[Element] -> Element", "Stack elements on top of each other.")
    ]

stylingContent =
    [ ("width", "Int -> Element -> Element", "Set the width of an element in pixels.")
    , ("height", "Int -> Element -> Element", "Set the width of an element in pixels.")
    , ("size", "Int -> Int -> Element -> Element", "Set the width and height of an element in pixels.")
    , ("opacity", "Number -> Element -> Element", "Set the opacity of an element. Requires a number between 0 and 1.")
    , ("color", "Color -> Element -> Element", "Set the background color of an element.")
    , ("link", "String -> Element -> Element", "Turn any element into a link.")
    ]

inspectingContent =
    [ ("widthOf", "Element -> Int", "Get the width of an element in pixels.")
    , ("heightOf", "Element -> Int", "Get the width of an element in pixels.")
    , ("sizeOf", "Element -> (Int,Int)", "Get the width and height of an element in pixels.")
    ]

positioningContent =
    [ ("spacer", "Int -> Int -> Element", "Create an empty element. Good for adding spaces.")
    , ("container", "Int -> Int -> Position -> Element -> Element", "Put an element in a container.")
    , ("topLeft, midLeft, bottomLeft, midTop, middle, midBottom, topRight, midRight, bottomRight", "Position", "Basic positions for an element in a container.")
    , ("topLeftAt, bottomLeftAt, middleAt, topRightAt, bottomRightAt", "Location -> Location -> Position", "Allows more flexible positioning of elements.")
    , ("absolute", "Int -> Location", "An absolute location in pixels.")
    , ("relative", "Float -> Location", "A relative location, given as a percentage.")
    ]

forms =
  [ ("toForm", "(Number,Number) -> Element -> Form", "Turn any Element into a Form")
  , ("sprite", "String -> Number -> Number -> (Number,Number) -> Form", "Create a sprite.")
  , ("move", "Number -> Number -> Form -> Form", "Translate a form by a given x and y offset.")
  , ("rotate", "Number -> Number -> Form -> Form", "Rotate a form by a given fraction of a full turn (not degrees or radians). For instance, (rotate 0.5) will rotate a form by a half turn.")
  , ("scale", "Number -> Form -> Form", "Scale a form by a given scale factor.")
  , ("isWithin", "(Number,Number) -> Form -> Bool", "Check to see if a point is within a given form. Forms can be concave (e.g. the letter C).")
  ]


shapes =
  [ ("rect", "Number -> Number -> (Number,Number) -> Shape", "Create a rectangle with a given width, height, and center position.")
  , ("oval", "Number -> Number -> (Number,Number) -> Shape", "Create an oval with a given width, height, and center position.")
  , ("circle", "Number -> (Number,Number) -> Shape", "Create a circle with a given radius and center position.")
  , ("ngon", "Number -> Number -> (Number,Number) -> Shape", "Create regular polygons with n sides. This function takes the number of sides, the radius (distance from the center to a vertex), and a center.")
  , ("polygon", "[(Number,Number)] -> (Number,Number) -> Shape", "Create an arbitrary polygon from an list of vertexes and a center point. The vertexes should be listed relative to the origin (0,0). They are then translated to the given center.")
  , ("filled", "Color -> Shape -> Form", "Fill a shape with a given color.")
  , ("outlined", "Color -> Shape -> Form", "Outline a shape with a given color.")
  , ("customOutline", "[Number] -> Color -> Shape -> Form", "Outline a shape with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  , ("textured", "String -> Shape -> Form", "Fill a shape with a given texture.")
  ]

lines =
  [ ("line", "[(Number,Number)] -> Line", "Create a line from an list of vertexes.")
  , ("segment", "(Number,Number) -> (Number,Number) -> Line", "Create a line segment with a start and end point.")
  , ("solid", "Color -> Line -> Form", "Create a solid line with a given color.")
  , ("dashed", "Color -> Line -> Form", "Create a dashed line with a given color.")
  , ("dotted", "Color -> Line -> Form", "Create a dotted line with a given color.")
  , ("customLine", "[Number] -> Color -> Line -> Form", "Create a custom line with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  ]

categories = 
  [ ("Visual Content", visualContent)
  , ("Text Content", textContent)
  , ("Combining Content", combiningContent)
  , ("Styling Content", stylingContent)
  , ("Inspecting Content", inspectingContent)
  , ("Positioning Content", positioningContent)
  , ("Collage", forms)
  , ("Collage: Shapes", shapes)
  , ("Collage: Lines", lines)
  ]

main = createDocs "Graphics.Element" categories