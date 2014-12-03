import Graphics.Collage (..)
import Graphics.Element (..)
import Text (plainText)


main : Element
main =
  collage 200 200
    [ rotate (degrees 20) (toForm (plainText "Any element can go here!"))
    ]