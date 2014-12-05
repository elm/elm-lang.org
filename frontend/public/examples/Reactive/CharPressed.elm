-- Click on the righthand screen and start pressing keys!

import Char
import Graphics.Element (..)
import Keyboard
import Signal
import Text (..)


main : Signal Element
main =
  Signal.map display Keyboard.lastPressed


display : Int -> Element
display keyCode =
  flow right
    [ plainText "The last key you pressed was: "
    , asText (Char.fromCode keyCode)
    ]