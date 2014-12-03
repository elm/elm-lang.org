import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)


main : Element
main =
  collage 300 300
    [ move (-10,0) (filled clearGrey (ngon 4 75))
    , move (50,10) (filled clearGrey (ngon 5 50))
    ]


clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

