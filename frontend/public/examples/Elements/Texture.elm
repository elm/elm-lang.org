import Graphics.Collage (..)
import Graphics.Element (..)


main : Element
main =
  collage 300 300
    [ textured "/stripes.jpg" (ngon 5 75)
    ]