
import Input
import Char

(field, input) = textField ""

scene inp =
  field `above` plainText ("Last input of all digits: " ++ inp)

main = lift scene (keepIf (all isDigit) "" input)

