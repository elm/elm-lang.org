
import Signal.Input

(field, input) = textField ""

isValid s = any ((==) 'a') s

scene inp =
  field `above` plainText ("Last input that contained an 'a' was: " ++ inp)

main = lift scene (keepIf isValid "" input)

