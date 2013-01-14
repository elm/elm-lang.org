
import Website.Skeleton
import Website.Tiles

addFolder folder = map (\(x,y) -> (x, y, folder))

intermediate = addFolder "Intermediate/"
  [ ("Slide Show", "SlideShow")
  , ("Graphs", "Plot")
  , ("Turtle", "Turtle")
  , ("Flickr API", "Flickr")
  , ("Light Box", "LightBox")
  , ("Stamps", "Stamps")
  , ("Keyboard & Animations", "MovingBox")
  , ("Quick Animations", "Slide")
  , ("Form Validation", "Form")
  , ("Diagrams", "Physics")
  , ("Fibonacci Tiles", "FibonacciTiles")
  , ("Analog Clock", "Clock")
  , ("Pascal's Triangle", "PascalsTriangle")
  , ("Web", "Web")
  ]

intro =  [markdown|

### Intermediate Examples

These examples bring together computation, graphics, and reactions to
build larger components.

<br/>

|]

content w =
  let tiles = tile w $ map toTile intermediate in
  width w intro `above` container w (heightOf tiles) midTop tiles

main = lift (skeleton content) Window.width
