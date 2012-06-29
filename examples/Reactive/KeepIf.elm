
import Signal.Input

(field, input) = textField ""

isValid s = length s > 5

scene inp =
  field `above` plainText ("Last valid input was: " ++ inp)

main = lift scene (keepIf isValid "" input)

