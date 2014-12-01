-- Click on the righthand screen and start pressing keys!

import Char
import Keyboard

display : Int -> Element
display keyCode =
    flow right
        [ plainText "The last key you pressed was: "
        , asText (Char.fromCode keyCode)
        ]

main : Signal Element
main = lift display Keyboard.lastPressed