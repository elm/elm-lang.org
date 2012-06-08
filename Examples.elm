
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
  , ("Centering"    , "Centering" , "Reactive/")
  , ("Shapes"       , "Shapes"    , "Elements/")
  , ("Light Box"    , "LightBox"  , "Intermediate/")
  , ("Graphs"       , "Plot"      , "Intermediate/")
  , ("Analog Clock" , "Clock"     , "Intermediate/")
  , ("This Page"    , "Examples"  , "../")
  , ("Abstract Data Types", "Tree", "Functional/")
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

content w =
  [ section "Learn by Example"
  , plainText $ "Elm's interactive editor allows you to learn Elm by seeing and modifying actual code. " ++
                "Right now there are two categories of examples:"
  , text $ link "/examples/Basic.elm" (toText "Basic") ++
           toText " &#8212; the basic building blocks of Elm"
  , text $ link "/examples/Intermediate.elm" (toText "Intermediate") ++
           toText " &#8212; building components with Elm"
  , text $ toText "To see a larger project, you can view all of the source " ++
           toText "code of this website on " ++
           link "https://github.com/evancz/elm-lang.org" (toText "github") ++
           toText ". If you want to see these files in the interactive editor, " ++
           toText "just insert " ++ monospace (toText "edit/") ++ toText " after this sites domain name. For instance, " ++
           link "/edit/Elm.elm" (toText "elm-lang.org/" ++ bold (toText "edit/") ++ toText "Elm.elm") ++
           toText "."
  , plainText "&nbsp;"
  , section "Quick Overview of Elm"
  , plainText "If you just want a brief overview, check out the following examples:"
  , width w . box 2 . tile w $ List.map toTile examples
  ]

exampleSets w = flow down . List.map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
