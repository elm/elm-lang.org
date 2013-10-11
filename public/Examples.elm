
import Website.Skeleton (skeleton)
import Website.ColorScheme (accent4)
import Website.Tiles (examples)
import Window

main = skeleton exampleSets <~ Window.dimensions

content w =
  let exs = [ ("Display",elements), ("React",reactive), ("Compute",functional) ]
  in  words :: map (subsection w) exs ++ [ intermediate, examples w intermediates, spacer w 30 ]

exampleSets w =
  flow down . map (width w) . intersperse (plainText " ") <| content w

words = [markdown|

# Examples

This page will help you *learn by example* as you read and modify
Elm code in the [online editor](/try). It is split into two sections:

 * [Basics](#basics) &ndash; small programs focused on showing one concept
 * [Intermediate](#intermediate) &ndash; larger examples that combine basic concepts

Remember to check the [Elm syntax reference][syntax] when you see new syntax!

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

## Basics

|]

intermediate = [markdown|

## Intermediate

|]

intermediates =
    [ [ "Mario", "Walk", "Pong", "Turtle" ]
    , [ "SlideShow", "Flickr", "Physics", "PieChart" ]
    , [ "Clock", "Stamps", "Slide", "Complements" ]
    , [ "Web", "Circles", "PascalsTriangle", "FibonacciTiles" ]
    ]

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm")
      f (n,xs) = (n, map add xs)
  in  map f lst

elements = addFolder "Elements/"
  [ ("Words",
        [ ("Text", "HelloWorld")
        , ("Markdown", "Markdown")
        ])
  , ("Images",
        [ ("Images", "Image")
        , ("Fitted", "FittedImage")
        , ("Cropped", "CroppedImage")
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
        ])
  , ("Positioning",
        [ ("Containers", "Position")
        , ("Spacers"   , "Spacer")
        ])
  , ("2D Shapes", [ ("Lines"     , "Lines")
                  , ("Shapes"    , "Shapes")
                  , ("Sprites"   , "Sprite")
                  , ("Elements"  , "ToForm")
                  , ("Transforms", "Transforms")
                  ])
  , ("2D Fills", [ ("Color"    , "Color")
                 , ("Linear Gradient", "LinearGradient")
                 , ("Radial Gradient", "RadialGradient")
                 , ("Texture"  , "Texture")
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
      [ ("Functions"  , "Anonymous")
      , ("Application", "Application")
      , ("Composition", "Composition")
      , ("Infix Ops"  , "Infix")
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
               , ("Yogi", "ResizeYogi")
               , ("Track", "Transforms")
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
  , ("Input",  [ ("Text", "TextField")
               , ("Password"  , "Password")
               , ("Checkbox", "CheckBox")
               , ("DropDown", "DropDown")
               , ("DropDown", "StringDropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("Http",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
               ])
  ]

example (name, loc) = Text.link ("/edit/examples/" ++ loc) (toText name)
toLinks (title, links) =
  flow right
   [ width 120 (plainText <| " " ++ title)
   , text . join (toText ", ") <| map example links
   ]

subsection w (name,info) =
  flow down . intersperse (spacer w 6) . map (width w) <|
    (text . bold <| toText name) :: map toLinks info ++ [spacer w 12]
