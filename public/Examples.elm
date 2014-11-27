import Text
import Website.Skeleton (skeleton)
import Website.ColorScheme (accent4)
import Website.Tiles as Tile
import Website.Widgets (headerFaces)
import Window

port title : String
port title = "Examples"

main = skeleton "Examples" body <~ Window.dimensions

body outer =
  let b = flow down <| intersperse (spacer 20 20) content
  in  container outer (heightOf b) middle b

content =
  let w = 600
      exs = [ ("Display",elements), ("React",reactive), ("Compute",functional) ]
  in  width w words ::
      map (subsection w) exs ++
      [ width w intermediate
      , Tile.examples w intermediates
      , width w projects
      ]

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

<h2 id="basics">Basics</h2>

|]

intermediate = [markdown|

<h2 id="intermediate">Intermediate</h2>

|]

projects = [markdown|

<h2 id="big-projects">Big Projects</h2>

These are all larger projects created with Elm. Fork them and use them
as templates for your own project!

#### Websites

 * [elm-todomvc](https://github.com/evancz/elm-todomvc) &mdash;
   todo list modelled on [TodoMVC](http://todomvc.com/)
 * [elm-lang.org](https://github.com/elm-lang/elm-lang.org) &mdash;
   front-end and back-end code for this website
 * [Reddit Time Machine](https://github.com/Dobiasd/RedditTimeMachine) &mdash;
   Check out what was up on reddit days/weeks/months ago. by Tobias Hermann
 * [EAN/UPC-A Barcode Generator](https://github.com/Dobiasd/Barcode-Generator) &mdash;
   with addon2/addon5 and real-time output, by Tobias Hermann
   
#### Games

 * [Tetris](https://github.com/jcollard/elmtris) &mdash;
   by Joe Collard
 * [Breakout](https://github.com/Dobiasd/Breakout#breakout--play-it) &mdash;
   by Tobias Hermann
 * [Maze](https://github.com/Dobiasd/Maze#maze--play-it) &mdash;
   by Tobias Hermann
 * [Celestia](https://github.com/johnpmayer/celestia) &mdash;
   modular spaceship game by John P. Mayer Jr
 * [Demoscene-Concentration](https://github.com/Dobiasd/Demoscene-Concentration) &mdash;
   the classical memory game with (simple) old-school demoscene effects, by Tobias Hermann

|]


intermediates =
    let ex = map Tile.intermediate
        gl = map Tile.webgl
    in
        [ ex [ "Mario", "Walk", "Pong", "Turtle" ]
        , ex [ "TextReverse", "Calculator", "Form", "Flickr" ]
        , ex [ "Clock", "Plot", "SlideShow", "PieChart" ]
        , gl [ "Triangle", "Cube", "Thwomp", "FirstPerson" ]
        , ex [ "Physics", "Stamps" ]
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
   [ width 150 (plainText <| "    " ++ title)
   , leftAligned << join (toText ", ") <| map example links
   ]

subsection w (name,info) =
  flow down << intersperse (spacer w 6) << map (width w) <|
    (tag name << leftAligned << typeface headerFaces << bold <| toText name) ::
    spacer 0 0 ::
    map toLinks info ++ [spacer w 12]
