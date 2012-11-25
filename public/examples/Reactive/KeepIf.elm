
(field, input) = Input.textField ""

scene inp =
  field `above` plainText ("Last input of all digits: " ++ inp)

main = lift scene (keepIf (all Char.isDigit) "" input)

