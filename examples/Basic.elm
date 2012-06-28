
import Data.List (intercalate)
import Website.Skeleton

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
        , ("Text"    , "Text")
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
  ,("Keyboard",[ ("Keys Down"  , "KeysDown")
               , ("Key Presses", "CharPressed")
               ])
  , ("Window", [ ("Size", "ResizePaint")
               , ("Centering", "Centering")
               ])
  , ("Time",   [ ("Before and After", "Between")
               , ("Every"           , "Every")
               , ("Clock"           , "Clock")
               ])
  , ("Input",  [ ("Text Fields", "TextField")
               , ("Field Validation", "Form")
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
         intercalate (fromString ", ") (map example links)

insertSpace lst = case lst of { x:xs -> x : rectangle 1 5 : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) : map toLinks info

content w =
  [ section "Basic Examples"
  , plainText $ "Each example listed below focuses on a single function or concept. Together, these " ++
                "examples should provide the basic building blocks needed to program in Elm."
  ] ++ map (subsection w) [ ("Display",elements), ("React",reactive), ("Compute",functional) ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
