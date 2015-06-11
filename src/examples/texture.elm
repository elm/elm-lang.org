import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


main : Element
main =
  collage 300 300
    [ textured "/stripes.jpg" (ngon 5 75)
    ]
