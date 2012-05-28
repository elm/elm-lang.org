map f xs = case xs of { x:xs -> f x : map f xs ; [] -> [] }
intersperse sep xs =
  case xs of { a:b:cs -> a : sep : intersperse sep (b:cs)
             ; a:[] -> [a] ; [] -> [] }
intercalate sep xs =
  case xs of { a:b:cs -> a ++ sep ++ intercalate sep (b:cs)
             ; a:[] -> a ; [] -> [] }

----------------------

button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . map button $
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

addSpaces = map (\f -> f ()) . intersperse (\x -> plainText "&nbsp;") . map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm") in
  let f (n,xs) = (n, map add xs) in
  map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Videos", "Video")
        ])
  , ("Formatting",
        [ ("Size"    , "Size")
        , ("Opacity" , "Opacity")
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

funExamples = ("",
  [ ("Blog"             , "Blog.elm")
  , ("Pascal's Triangle", "PascalsTriangle.elm")
  , ("Fibonacci Tiles"  , "FibonacciTiles.elm")
  ])

example (name, loc) = link ("/edit/examples/" ++ loc) (fromString name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         intercalate (fromString ", ") (map example links)

insertSpace lst = case lst of { x:xs -> x : (height 5 $ plainText "") : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) : map toLinks info

intro = [ section "Learn by Example"
        , plainText $ "Any framework for the creation of Graphical User Interfaces (GUIs) must " ++
                      "be able to (1) display information on screen, (2) react to user input and " ++
                      "system input, and (3) run computations on data. The following examples are " ++
                      "divided into these three categories, allowing you to learn Elm by seeing " ++
                      "and modifying actual code in an interactive editor."
        , text $ toText "If you just want a brief overview, check out the following examples: " ++
                 link "/edit/examples/Elements/FlowDown2.elm" (toText "simple displays") ++ toText "; " ++
                 link "/edit/examples/Reactive/Centering.elm" (toText "centering") ++ toText " and " ++
                 link "/edit/examples/Elements/Layers.elm" (toText "layering") ++ toText " elements; complex " ++
                 link "/edit/examples/Elements/Shapes.elm" (toText "shapes") ++ toText " and " ++
                 link "/edit/examples/Elements/Lines.elm" (toText "lines") ++ toText "; " ++
                 link "/edit/examples/Reactive/Transforms.elm" (toText "mouse input") ++ toText "; and " ++
                 link "/edit/examples/Reactive/Clock.elm" (toText "9-line analog clock") ++ toText "."
        ]
links w = map (subsection w)
        [ ("Display",elements), ("React",reactive), ("Compute",functional) ]

exampleSets w = flow down . map (width w) . addSpaces $ intro ++ links w

main = lift (skeleton exampleSets) Window.width
