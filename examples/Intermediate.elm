
import Website.Skeleton
import Website.Tiles

section = text . bold . Text.height (5/4) . toText

addFolder folder = map (\(x,y) -> (x, y, folder))

intermediate = addFolder "Intermediate/"
  [ ("Slide Show"   , "SlideShow")
  , ("Graphs"       , "Plot")
  , ("Sign-up Form" , "Form")
  , ("Light Box"    , "LightBox")
  , ("Stamps"       , "Stamps")
  , ("Fibonacci Tiles"  , "FibonacciTiles")
  , ("Analog Clock" , "Clock")
  , ("Pascal's Triangle", "PascalsTriangle")
  , ("Web" , "Web")
  ]

content w =
  [ section "Intermediate Examples"
  , plainText $ "These examples bring together display, reaction, and computation to " ++
                "illustrate how Elm can create useful components."
  , width w . box 2 . tile w $ map toTile intermediate
  ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
