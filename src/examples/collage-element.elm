import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


main : Element
main =
  collage 200 200
    [ rotate (degrees 20) (toForm (show "Any element can go here!"))
    ]