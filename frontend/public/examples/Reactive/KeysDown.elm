
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Graphics.Element exposing (..)
import Keyboard
import Text exposing (..)


display : List Int -> Element
display keyCodes =
  plainText "You are holding down the following keys: " `beside` asText keyCodes


main : Varying Element
main =
  Varying.map display Keyboard.keysDown