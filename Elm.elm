
import Website.Skeleton
import Website.Tiles

section = text . bold . Text.height (5/4) . toText

examples =
  [ ("Layout"       , "FlowDown2" , "Elements/")
  , ("Shapes"       , "Shapes"    , "Elements/")
  , ("Analog Clock" , "Clock"     , "Intermediate/")
  , ("Slide Show"   , "SlideShow" , "Intermediate/")
  , ("Form Validation", "Form"    , "Intermediate/")
  ]


info w = flow down . map (width w) . addSpaces $
  [ section "The Elm Programming Language"
  , text $ toText "The Elm programming language aims to make web development " ++
           toText "more pleasant. Elm is a type-safe, functional reactive language " ++
           toText "that compiles to HTML, CSS, and JavaScript. You can start coding " ++
           toText "in Elm without any install or setup with Elm's " ++
           link "/edit/examples/Reactive/Transforms.elm" (toText "interactive editor") ++
           toText ", so start learning Elm by " ++
           link "/Examples.elm" (toText "example") ++ toText ":"
  , width w . box 2 . tile w $ map toTile examples
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
  , text $ toText "Version 0.3 of the compiler is now available. Details " ++ link "http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/" (toText "here") ++ toText "."
  , text $ toText "Version 0.2 of the compiler was recently released. Error messages have been significantly improved, " ++
           toText "basic optimizations are performed, noscript tags are generated, " ++
           toText "there is Haskell integration (to serve code with Yesod, Snap, HAppStack, etc.), " ++
           toText "pattern matching has been improved, and some minor bugs have been fixed. " ++
           toText "Upgrade with the command " ++ monospace (toText "cabal install elm-server") ++
           toText " or see the " ++ link "/Download.elm" (toText "downloads page") ++ toText " to get started from scratch."
  , text $ toText "New examples are available " ++
           link "/examples/Intermediate.elm" (toText "here") ++
           toText ". These larger and more complex examples show how Elm can create mid-size components."
  , text $ toText "See the Elm " ++
           link "https://groups.google.com/forum/?fromgroups#!forum/elm-discuss" (toText "mailing list") ++
           toText " for questions, announcements, and discussion. You can contact me directly at info (at) elm-lang (dot) org."
  ]
 
main = lift (skeleton info) Window.width
