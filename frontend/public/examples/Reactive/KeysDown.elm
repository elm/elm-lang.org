
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Graphics.Element exposing (..)
import Keyboard


display : List Int -> Element
display keyCodes =
  show "You are holding down the following keys: " `beside` show keyCodes


main : Signal.Signal Element
main =
  Signal.map display Keyboard.keysDown