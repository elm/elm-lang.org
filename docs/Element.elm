
import Website.Docs

textContent =
    [ ("plainText", "String -> Element", "Display unstyled strings.")
    , ("text", "Text -> Element", "Display styled text.")
    , ("centeredText", "Text -> Element", "Display centered, styled text.")
    , ("rightedText", "Text -> Element", "Display right justified, styled text.")
    , ("justifiedText", "Text -> Element", "Display justified, styled text.")
    ]

visualContent =
    [ ("image", "String -> Element", "Display images.")
    , ("video", "String -> Element", "Display videos.")
    , ("collage", "Int -> Int -> [Form] -> Element", "Renders the scene specified by the given width, height, and list of primitive graphical forms.")
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
    ]

positioningContent =
    [ ("box", "Int -> Element -> Element", "Create a box around an element. The given number specifies where the inner element will float within the box (i.e. 1 is top left, 2 is top middle, 3 is top right, ...). Changes to size now only affect the outer box.") ]

categories = 
  [ ("Text Content", textContent)
  , ("Visual Content", visualContent)
  , ("Combining Content", combiningContent)
  , ("Styling Content", stylingContent)
  , ("Positioning Content", positioningContent)
  ]

main = createDocs "Element: Displaying Content" categories