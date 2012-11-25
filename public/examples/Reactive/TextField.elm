
(fld, txt) = Input.textField "Type here!" in

main = lift2 above (constant fld) (lift showLen txt)

showLen n =
  text . monospace . toText $
  "The string has " ++ show (length n) ++ " characters."


-- Note: textField will someday use its string argument as greyed out
-- ghost text that is only visible when the textField is empty. Right
-- now the string is just ignored.