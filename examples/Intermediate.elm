
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

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm", "/screenshot/" ++ y ++ ".jpg") in
  List.map add lst

intermediate = addFolder "Intermediate/"
  [ ("Slide Show"   , "SlideShow")
  , ("Graphs"       , "Plot")
  , ("Analog Clock" , "Clock")
  , ("Light Box"    , "LightBox")
  , ("Stamps"       , "Stamps")
  , ("Fibonacci Tiles"  , "FibonacciTiles")
  , ("Pascal's Triangle", "PascalsTriangle")
  , ("Web" , "Web")
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
  [ section "Intermediate Examples"
  , plainText $ "These examples bring together display, reaction, and computation to " ++
                "illustrate how Elm can create useful components."
  , width w . box 2 . tile w $ List.map toTile intermediate
  ]

exampleSets w = flow down . List.map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
