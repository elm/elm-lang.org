import Char
import String
import Graphics.Input as Input

(field, input) = Input.field "numbers"

scene field inp = field `above` plainText ("Last input of all digits: " ++ inp)

main = lift2 scene field (keepIf (String.all Char.isDigit) "" input)

