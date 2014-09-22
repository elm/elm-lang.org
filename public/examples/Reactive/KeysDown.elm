
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Keyboard

display : [Int] -> Element
display keyCodes =
    plainText "You are holding down the following keys: " `beside` asText keyCodes


main : Signal Element
main = lift display Keyboard.keysDown