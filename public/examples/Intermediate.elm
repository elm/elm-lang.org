
import Website.Skeleton (skeleton)
import Website.Tiles (toTile, tile)
import Window

addFolder folder = map (\(x,y) -> (x, y, folder))

intermediate = addFolder "Intermediate/"
  [ ("Slide Show", "SlideShow")
  , ("Turtle", "Turtle")
  , ("Side-Scroller", "Mario")
  , ("Adventure", "Walk")
  , ("Pong", "Pong")
  , ("Analog Clock", "Clock")
  , ("Diagrams", "Physics")
  , ("Quick Animations", "Slide")
  , ("Stamps", "Stamps")
  , ("Pascal's Triangle", "PascalsTriangle")
  , ("Fibonacci Tiles", "FibonacciTiles")
  , ("Web", "Web")
  , ("Circles", "Circles")
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
