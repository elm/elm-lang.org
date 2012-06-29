
import Website.Skeleton
import Website.Tiles

section = text . bold . Text.height (5/4) . toText

examples =
  [ ("Layout"       , "FlowDown2" , "Elements/")
  , ("Centering"    , "Centering" , "Reactive/")
  , ("Shapes"       , "Shapes"    , "Elements/")
  , ("Abstract Data Types", "Tree", "Functional/")
  , ("Analog Clock" , "Clock"     , "Intermediate/")
  , ("Light Box"    , "LightBox"  , "Intermediate/")
  , ("Graphs"       , "Plot"      , "Intermediate/")
  , ("Form Validation", "Form"      , "Intermediate/")
  , ("This Page"    , "Examples"  , "../")
  ]

content w =
  [ section "Learn by Example"
  , plainText $ "Elm's interactive editor allows you to learn Elm by seeing and modifying actual code. " ++
                "Right now there are two categories of examples:"
  , text $ link "/examples/Basic.elm" (toText "Basic") ++
           toText " &#8212; the basic building blocks of Elm"
  , text $ link "/examples/Intermediate.elm" (toText "Intermediate") ++
           toText " &#8212; building components with Elm"
  , text $ toText "There are also some examples of Elm's integration with JavaScript, Yesod, or HAppStack (available " ++
           link "https://github.com/evancz/Elm/tree/master/Examples" (toText "here") ++
           toText "). For more details on using Elm and JavaScript together, see this " ++
           link "http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5javascript-integration-signal-filters-and-more/" (toText "announcement") ++
           toText ". The most exciting examples of JavaScript integration are probably regulating frame rate (" ++
           link "https://github.com/evancz/Elm/tree/master/Examples/elm-js/FrameRate/" (toText "source") ++
           toText ", " ++
           link "/misc/FrameRate.html" (toText "result") ++
           toText ") and use of the OpenStreetMap API (" ++
           link "https://github.com/evancz/Elm/tree/master/Examples/elm-js/Maps/" (toText "source") ++
           toText ", " ++
           link "/misc/Map.html" (toText "result") ++
           toText ")."
  , text $ toText "This website was written almost entirely in Elm, so if you want to see a larger example, you can browse the source code on " ++
           link "https://github.com/evancz/elm-lang.org" (toText "github") ++
           toText ". The download links are " ++
           link "https://github.com/evancz/elm-lang.org/downloads" (toText "here") ++
           toText ". The download also includes all of the examples on this site, which might " ++
           toText "be nice if you prefer to work on your own machine."
  , text $ toText "If you want to see the source code of a particular page in the online editor, " ++
           toText "just insert " ++ monospace (toText "edit/") ++ toText " after this sites domain name. For instance, " ++
           link "/edit/Examples.elm" (toText "elm-lang.org/" ++ bold (toText "edit/") ++ toText "Examples.elm") ++
           toText " is the source code for this page."
  , plainText "&nbsp;"
  , section "Quick Overview of Elm"
  , plainText "If you just want a brief overview, check out the following examples:"
  , width w . box 2 . tile w $ map toTile examples
  ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
