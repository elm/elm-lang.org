import Graphics.Collage (..)
import Graphics.Element (..)


main : Element
main =
  collage 200 200
    [ sprite 150 150 (10,10) "/yogi.jpg" ]
