
import Website.Skeleton (skeleton)
import Website.ColorScheme (accent4)
import Website.Tiles (examples)
import Window

main = skeleton exampleSets <~ Window.dimensions

content w =
  let exs = [ ("Display",elements), ("React",reactive), ("Compute",functional) ]
  in  words :: map (subsection w) exs ++ [ intermediate, examples w intermediates, projects ]

exampleSets w =
  flow down . map (width w) . intersperse (plainText " ") <| content w

words = [markdown|

# Examples

This page will help you *learn by example* as you read and modify
Elm code in the [online editor](/try). It is split into sections
that will help you grow from beginner to expert:

 * [Basics](#basics) &ndash; small programs focused on showing one concept
 * [Intermediate](#intermediate) &ndash; larger examples that combine basic concepts
 * [Open Source Projects](#open-source-projects) &ndash; bigger things written in Elm

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

## Open Source Projects

These are all larger projects created with Elm. I hesitate to call them
&ldquo;example&rdquo; projects, but they are all good starting points
for building on top of or retrofitting for your purposes.

#### Games

 * [Tetris](https://github.com/jcollard/elmtris) &ndash;
   by Joe Collard

 * [Breakout](https://github.com/Dobiasd/Breakout#breakout--play-it) &ndash;
   by Tobias Hermann

 * [Maze](https://github.com/Dobiasd/Maze#maze--play-it) &ndash;
   by Tobias Hermann

 * [Celestia](https://github.com/johnpmayer/celestia) &ndash;
   modular spaceship game by John P. Mayer Jr

#### Websites

 * [TodoFRP](https://github.com/evancz/TodoFRP) &ndash;
   a [TodoMVC](http://todomvc.com/)-style todo list

 * [elm-lang.org](https://github.com/evancz/elm-lang.org) &ndash;
   written mostly in Elm, good template for your own site

 * [docs.elm-lang.org](https://github.com/evancz/docs.elm-lang.org) &ndash;
   written entirely in Elm. Check out [instant search](http://docs.elm-lang.org/)
   
|]


intermediates =
    [ [ "Mario", "Walk", "Pong", "Turtle" ]
    , [ "SlideShow", "Flickr", "Physics", "PieChart" ]
    , [ "Plot", "Clock", "Stamps", "Slide" ]
    , [ "Complements", "Tracer", "PascalsTriangle", "FibonacciTiles" ]
    , [ "Web", "Circles" ]
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
               , ("Drop", "StringDropDown")
               , ("General Drop", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("Http",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
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
   , text . join (toText ", ") <| map example links
   ]

subsection w (name,info) =
  flow down . intersperse (spacer w 6) . map (width w) <|
    (tag name . text . bold <| toText name) :: map toLinks info ++ [spacer w 12]
