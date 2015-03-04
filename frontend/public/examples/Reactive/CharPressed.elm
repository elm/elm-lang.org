-- Click on the righthand screen and start pressing keys!

import Char
import Graphics.Element exposing (..)
import Keyboard
import Text exposing (..)


main : Varying Element
main =
  Varying.map display Keyboard.lastPressed


display : Int -> Element
display keyCode =
  flow right
    [ plainText "The last key you pressed was: "
    , asText (Char.fromCode keyCode)
    ]