
import List (intercalate,intersperse)
import Website.Skeleton
import Graphics.Text as Text

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm") in
  let f (n,xs) = (n, map add xs) in
  map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Fitted Images", "FittedImage")
        , ("Videos", "Video")
        , ("Markdown", "Markdown")
        ])
  , ("Formatting",
        [ ("Size"    , "Size")
        , ("Opacity" , "Opacity")
        , ("Text"    , "Text")
        , ("Typeface", "Typeface")
        ])
  , ("Layout",
        [ ("Simple Flow", "FlowDown1a")
        , ("Flow Down"  , "FlowDown2")
        , ("Layers"     , "Layers")
        , ("Positioning", "Position")
        , ("Spacers"    , "Spacer")
        ])
  , ("Collage", [ ("Lines"     , "Lines")
                , ("Shapes"    , "Shapes")
                , ("Sprites"   , "Sprite")
                , ("Elements"  , "ToForm")
                , ("Colors"    , "Color")
                , ("Textures"  , "Texture")
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
               , ("Passwords"  , "Password")
               , ("Check Boxes", "CheckBox")
               , ("String Drop Down", "StringDropDown")
               , ("Drop Down", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("HTTP",   [ ("Zip Codes (via AJAX requests)", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
               , ("Keep If", "KeepIf")
               , ("Drop Repeats", "DropRepeats")
               ])
  ]

example (name, loc) = Text.link ("/edit/examples/" ++ loc) (toText name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         intercalate (fromString ", ") (map example links)

insertSpace lst = case lst of { x:xs -> x : spacer 1 5 : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) : map toLinks info

words = [markdown|

### Basic Examples

Each example listed below focuses on a single function or concept.
These examples demonstrate all of the basic building blocks of Elm.

|]

content w =
  words : map (subsection w) [ ("Display",elements), ("React",reactive), ("Compute",functional) ]

exampleSets w = flow down . map (width w) . intersperse (plainText "&nbsp;") $ content w

main = lift (skeleton exampleSets) Window.width
