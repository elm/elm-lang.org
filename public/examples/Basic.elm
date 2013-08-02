
import Website.Skeleton (skeleton)
import Website.ColorScheme (accent4)
import Window

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm")
      f (n,xs) = (n, map add xs)
  in  map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Fitted Images", "FittedImage")
        , ("Cropped Images", "CroppedImage")
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
  , ("2D Shapes", [ ("Lines"     , "Lines")
                  , ("Shapes"    , "Shapes")
                  , ("Sprites"   , "Sprite")
                  , ("Elements"  , "ToForm")
                  , ("Transforms", "Transforms")
                  ])
  , ("2D Fills", [ ("Colors"    , "Color")
                 , ("Linear Gradient", "LinearGradient")
                 , ("Radial Gradient", "RadialGradient")
                 , ("Textures"  , "Texture")
                 ])
  ]


functional = addFolder "Functional/"
  [ ("Recursion",
      [ ("Factorial"  , "Factorial")
      , ("List Length", "Length")
      , ("Zip"        , "Zip")
      , ("Quick Sort" , "QuickSort")
      ])
  , ("Functions",
      [ ("Anonymous Functions", "Anonymous")
      , ("Application"        , "Application")
      , ("Composition"        , "Composition")
      , ("Infix Operators"    , "Infix")
      ])
  , ("Higher-Order",
      [ ("Map"    , "Map")
      , ("Fold"   , "Sum")
      , ("Filter" , "Filter")
      , ("ZipWith", "ZipWith")
      ])
  , ("Data Types",
      [ ("Maybe", "Maybe")
      , ("Boolean Expressions", "BooleanExpressions")
      , ("Tree", "Tree")
      ])
  , ("Libraries",
        [ ("Either", "Either")
        , ("Dict", "Dict")
        , ("Set", "Set")
        ])
  ]

reactive = addFolder "Reactive/"
  [ ("Mouse",  [ ("Position", "Position")
               , ("Presses"    , "IsDown")
               , ("Clicks"    , "CountClicks")
               , ("Position+Image", "ResizeYogi")
               , ("Position+Collage"    , "Transforms")
               ])
  ,("Keyboard",[ ("Arrows"     , "Arrows")
               , ("wasd"       , "Wasd")
               , ("Keys Down"  , "KeysDown")
               , ("Key Presses", "CharPressed")
               ])
  , ("Touch",  [ ("Raw", "Touches")
               , ("Touches", "Touch")
               , ("Taps", "Taps")
               , ("Draw", "Draw")
               ])
  , ("Window", [ ("Size", "ResizePaint")
               , ("Centering", "Centering")
               ])
  , ("Time",   [ ("FPS"     , "Fps")
               , ("FPS when", "FpsWhen")
               , ("Every"   , "Every")
               , ("Clock"   , "Clock")
               ])
  , ("Input",  [ ("Text Fields", "TextField")
               , ("Passwords"  , "Password")
               , ("Check Boxes", "CheckBox")
               , ("Drop Down", "DropDown")
               , ("String Drop Down", "StringDropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("Http",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
--               , ("Keep If", "KeepIf")
               ])
  ]

example (name, loc) = Text.link ("/edit/examples/" ++ loc) (toText name)
toLinks (title, links) =
  flow right
   [ width 130 (plainText <| "   " ++ title)
   , text . join (bold . Text.color accent4 <| toText "  &middot;  ") <|
     map example links
   ]

insertSpace lst = case lst of { x::xs -> x :: spacer 1 5 :: xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . intersperse (spacer 1 1) . map (width w) <|
    (text . bold <| toText name) :: map toLinks info

words = [markdown|

### Basic Examples

Each example listed below focuses on a single function or concept.
These examples demonstrate all of the basic building blocks of Elm.
For more details on the syntax of Elm, take a look at [The Syntax of Elm][syntax].

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

|]

content w =
  let exs = [ ("Display",elements), ("React",reactive), ("Compute",functional) ]
  in  words :: map (subsection w) exs

exampleSets w =
  flow down . map (width w) . intersperse (plainText " ") <| content w

main = skeleton exampleSets <~ Window.width
