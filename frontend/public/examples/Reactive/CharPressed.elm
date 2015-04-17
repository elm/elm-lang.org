-- Click on the righthand screen and start pressing keys!

import Char
import Graphics.Element exposing (..)
import Keyboard


main : Signal Element
main =
  Signal.map display Keyboard.presses


display : Int -> Element
display keyCode =
  flow right
    [ show "The last key you pressed was: "
    , show (Char.fromCode keyCode)
    ]