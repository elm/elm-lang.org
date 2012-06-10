button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (245/255) (245/255) (245/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , width outer . box 2 $ plainText "&nbsp;" `above` body inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces px = List.map (\f -> f ()) . List.intersperse (\x -> height px $ plainText "") . List.map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

darkerGrey = rgb (114/255) (124/255) (129/255)
entry w (name, type, desc) =
  flow down . List.map (width w) $
    [ text . monospace . toText $ name ++ " :: " ++ type
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . List.map (width w) $ (text . bold $ toText name) : List.map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : List.map (group w) cats in
  lift (skeleton f) Window.width

----------------------

textContent =
    [ ("plainText", "String -> Element", "Display unstyled strings.")
    , ("text", "Text -> Element", "Display styled text.")
    , ("centeredText", "Text -> Element", "Display centered, styled text.")
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