
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
                "There are a couple categories of examples for designed to build certain skills:"
  , text $ link "/examples/Basic.elm" (toText "Basic") ++
           toText " &#8212; the basic building blocks of Elm"
  , text $ link "/examples/Intermediate.elm" (toText "Intermediate") ++
           toText " &#8212; building components with Elm"
  , text $ link "/examples/ElmJS.elm" (toText "Elm + JavaScript") ++
           toText " &#8212; use existing JavaScript libraries, features, etc."
  , text $ link "/examples/ElmHaskell.elm" (toText "Elm + Haskell") ++
           toText " &#8212; serve Elm code without leaving Haskell"
  , text $ link "/examples/ThisWebsite.elm" (toText "This Website") ++
           toText " &#8212; the Elm source code for this site"
  , text $ link "/blog/games-in-elm/part-0/Making-Pong.html" (toText "Elm for Games") ++
           toText " &#8212; how to make games in Elm"
  , rectangle 1 5
  , section "Quick Overview of Elm"
  , plainText "If you just want a brief overview, check out the following examples:"
  , width w . box 2 . tile w $ map toTile examples
  ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
