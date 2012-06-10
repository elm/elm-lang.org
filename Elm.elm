
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

format lst =
  let add (x,y,z) = (x, z ++ y ++ ".elm", "/screenshot/" ++ y ++ ".jpg") in
  List.map add lst

examples = format
  [ ("Layout"       , "FlowDown2" , "Elements/")
  , ("Shapes"       , "Shapes"    , "Elements/")
  , ("Analog Clock" , "Clock"     , "Intermediate/")
  , ("Slide Show"   , "SlideShow" , "Intermediate/")
  ]

tileSize = 130

toTile (name, ex, pic) =
  let x = tileSize in
  Element.link ("/edit/examples/" ++ ex) $ flow down
  [ size x x . box 5 . size (x-10) (x-10) $ image pic
  , width x . centeredText $ toText name
  ]

take n lst = if n <= 0 then [] else
             case lst of { x:xs -> x : take (n-1) xs ; [] -> [] }
drop n lst = if n <= 0 then lst else
             case lst of { x:xs -> drop (n-1) xs ; [] -> [] }
groups n lst =
  case lst of
  { [] -> [] ; x:xs -> take n lst : groups n (drop n lst) }

tile w tiles =
  flow down . addSpaces . List.map (flow right) $ groups (w/tileSize-1) tiles

info w = flow down . List.map (width w) . addSpaces $
  [ section "The Elm Programming Language"
  , text $ toText "The Elm programming language aims to make web development " ++
           toText "more pleasant. Elm is a type-safe, functional reactive language " ++
           toText "that compiles to HTML, CSS, and JavaScript. You can start coding " ++
           toText "in Elm without any install or setup with Elm's " ++
           link "/edit/examples/Reactive/Transforms.elm" (toText "interactive editor") ++
           toText ", so start learning Elm by " ++
           link "/Examples.elm" (toText "example") ++ toText ":"
  , width w . box 2 . tile w $ List.map toTile examples
  , text $ toText "Elm's major distinguishing features are support for " ++
           link "http://en.wikipedia.org/wiki/Reactive_programming" (toText "reactive programming") ++
           toText " and its focus on graphical user interfaces. Elm is also " ++
           link "http://en.wikipedia.org/wiki/Evaluation_strategy" (toText "call-by-value") ++ toText " and " ++
           link "http://en.wikipedia.org/wiki/Strong_typing" (toText "strongly") ++ toText " / " ++
           link "http://en.wikipedia.org/wiki/Type_system#Static_typing" (toText "statically") ++ toText " typed with " ++
           link "http://en.wikipedia.org/wiki/Type_inference" (toText "type inference") ++
           toText ", so those of you already familiar with languages like Haskell or ML should be quite comfortable."
  , plainText $ "The language features mentioned above help catch errors in your code at compile time, but there are " ++
                "some errors that are external to your code, such as browser incompatabilities. By using HTML, CSS, and " ++
                "JavaScript as an assembly language, Elm can avoid some of these problems."
  , plainText "&nbsp;"
  , section "Status Update"
  , text $ toText "Version 0.2 of the compiler was recently released. Error messages have been significantly improved, " ++
           toText "basic optimizations are performed, noscript tags are generated, " ++
           toText "there is Haskell integration (to serve code with Yesod, Snap, HAppStack, etc.), " ++
           toText "pattern matching has been improved, and some minor bugs have been fixed. " ++
           toText "Upgrade with the command " ++ monospace (toText "cabal install elm-server") ++
           toText " or see the " ++ link "/Download.elm" (toText "downloads page") ++ toText " to get started from scratch."
  , text $ toText "New examples are available " ++
           link "/examples/Intermediate.elm" (toText "here") ++
           toText ". These larger and more complex examples show how Elm can create mid-size components."
  , plainText "You can contact me at info (at) elm-lang (dot) org."
  ]
 
main = lift (skeleton info) Window.width
