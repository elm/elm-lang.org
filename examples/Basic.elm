
button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (240/255) (241/255) (244/255)
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

addSpaces = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm") in
  let f (n,xs) = (n, List.map add xs) in
  List.map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Videos", "Video")
        ])
  , ("Formatting",
        [ ("Size"    , "Size")
        , ("Opacity" , "Opacity")
--        , ("Text"    , "Text")
        ])
  , ("Layout",
        [ ("Simple Flow", "FlowDown1a")
        , ("Flow Down"  , "FlowDown2")
        , ("Layers"     , "Layers")
        ])
  , ("Collage", [ ("Lines"     , "Lines")
                , ("Shapes"    , "Shapes")
                , ("Colors"    , "Color")
                , ("Transforms", "Transforms")
                ])
  ]


functional = addFolder "Functional/"
  [ ("Recursion",
        [ ("Factorial"  , "Factorial")
        , ("List Length", "Length")
        , ("Zip"        , "Zip")
        , ("Quick Sort" , "QuickSort")
        ])
  , ("Functions as Values",
        [ ("Anonymous Functions", "Anonymous")
        , ("Application"        , "Application")
        , ("Composition"        , "Composition")
        , ("Infix Operators"    , "Infix")
        ])
  , ("Higher-Order Functions",
        [ ("Map"    , "Map")
        , ("Fold"   , "Sum")
        , ("Filter" , "Filter")
        , ("ZipWith", "ZipWith")
        ])
  , ("Abstract Data Types",
        [ ("Maybe", "Maybe")
        , ("Boolean Expressions", "BooleanExpressions")
        , ("Tree", "Tree")
        ])
  ]

reactive = addFolder "Reactive/"
  [ ("Mouse",  [ ("Position", "Position")
               , ("Presses"    , "IsDown")
               , ("Clicks"    , "CountClicks")
               , ("Position+Image", "ResizeYogi")
               , ("Position+Collage"    , "Transforms")
               -- , ("Hover"     , "IsAbove")
               ])
  , ("Window", [ ("Size", "ResizePaint")
               , ("Centering", "Centering")
               ])
  , ("Time",   [ ("Before and After", "Between")
               , ("Every"           , "Every")
               , ("Clock"           , "Clock")
               ])
  , ("Input",  [ ("Text Fields", "TextField")
               , ("Passwords"  , "Password")
               , ("Check Boxes", "CheckBox")
               , ("String Drop Down", "StringDropDown")
               , ("Drop Down", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("HTTP", [ ("Zip Codes (via AJAX requests)", "ZipCodes") ])
  ]

example (name, loc) = link ("/edit/examples/" ++ loc) (fromString name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         List.intercalate (fromString ", ") (List.map example links)

insertSpace lst = case lst of { x:xs -> x : (height 5 $ plainText "") : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . List.map (width w) $
    (text . bold $ toText name) : List.map toLinks info

content w =
  [ section "Basic Examples"
  , plainText $ "Each example listed below focuses on a single function or concept. Together, these " ++
                "examples should provide the basic building blocks needed to program in Elm."
  ] ++ List.map (subsection w) [ ("Display",elements), ("React",reactive), ("Compute",functional) ]

exampleSets w = flow down . List.map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
