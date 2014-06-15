import Text
import Website.Skeleton (skeleton)
import Website.ColorScheme (accent4)
import Website.Tiles as Tile
import Window

main = skeleton exampleSets <~ Window.dimensions

content w =
  let exs = [ ("Display",elements), ("React",reactive), ("Compute",functional) ]
  in  words :: map (subsection w) exs ++ [ intermediate
                                         , Tile.examples w intermediates
                                         , projects
                                         ]

exampleSets w =
  flow down . map (width w) . intersperse (plainText " ") <| content w

words = [markdown|

# Examples

This page will help you *learn by example* as you read and modify
Elm code in the [online editor](/try). It is split into sections
that will help you grow from beginner to expert:

 * [Basics](#basics) &mdash; small programs focused on showing one concept
 * [Intermediate](#intermediate) &mdash; larger examples that combine basic concepts
 * [Big Projects](#big-projects) &mdash; websites and games written in Elm

Remember to check the [Elm syntax reference][syntax] when you see new syntax!
See the [learning resources](/Learn.elm) if you want to learn the fundamentals
of the language *not* by example. The [library documentation](/Libraries.elm)
is a very information dense resource once you become familiar with Elm.

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

## Basics

|]

intermediate = [markdown|

## Intermediate

|]

projects = [markdown|

## Big Projects

These are all larger projects created with Elm. Fork them and use them
as templates for your own project!

#### Websites

 * [TodoFRP](https://github.com/evancz/TodoFRP) &mdash;
   todo list modelled on [TodoMVC](http://todomvc.com/)
 * [elm-lang.org](https://github.com/elm-lang/elm-lang.org) &mdash;
   good template for your own website
 * [library.elm-lang.org](https://github.com/elm-lang/elm-get) &mdash;
   pretty and easily implements [contextual search](http://library.elm-lang.org)
   
#### Games

 * [Tetris](https://github.com/jcollard/elmtris) &mdash;
   by Joe Collard
 * [Breakout](https://github.com/Dobiasd/Breakout#breakout--play-it) &mdash;
   by Tobias Hermann
 * [Maze](https://github.com/Dobiasd/Maze#maze--play-it) &mdash;
   by Tobias Hermann
 * [Celestia](https://github.com/johnpmayer/celestia) &mdash;
   modular spaceship game by John P. Mayer Jr

|]


intermediates =
    let ex = map Tile.intermediate
        gl = map Tile.webgl
    in
        [ ex [ "Mario", "Walk", "Pong", "Turtle" ]
        , ex [ "TextReverse", "Calculator", "Form", "Flickr" ]
        , ex [ "Physics", "Plot", "PieChart", "SlideShow" ]
        , ex [ "Clock", "Tracer", "Slide", "Stamps" ]
        , ex [ "Complements", "PascalsTriangle", "Web", "FibonacciTiles" ]
        , gl [ "Triangle", "Cube", "Thwomp", "FirstPerson" ]
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
                  , ("Elements"  , "ToForm")
                  , ("Transforms", "Transforms")
                  ])
  , ("2D Fills", [ ("Color"    , "Color")
                 , ("Gradient", "LinearGradient")
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
               ])
  , ("Input",  [ ("Text", "TextField")
               , ("Password"  , "Password")
               , ("Checkbox", "CheckBox")
               , ("Drop Down", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("Http",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
               , ("Numbers Only", "KeepIf")
               ])
  , ("Ports",  [ ("Logging","Log")
               , ("Set Title","Title")
               , ("Redirect","Redirect")
               ])
  ]

example (name, loc) = Text.link ("/edit/examples/" ++ loc) (toText name)
toLinks (title, links) =
  flow right
   [ width 120 (plainText <| " " ++ title)
   , leftAligned . join (toText ", ") <| map example links
   ]

subsection w (name,info) =
  flow down . intersperse (spacer w 6) . map (width w) <|
    (tag name . leftAligned . bold <| toText name) :: map toLinks info ++ [spacer w 12]
