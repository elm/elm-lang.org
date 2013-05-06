-- Click on the righthand screen and start pressing keys!

import Char
import Keyboard

display code =
    plainText "The last key you pressed was: "
    `beside`
    asText (Char.fromCode code)

main = lift display Keyboard.lastKey