
main = display (Input.textField "")

display (fld, txt) =
  lift2 below (constant fld) (lift showLen txt)

showLen n =
  text . monospace $
  toText "The string has " ++ show (List.length n) ++ toText " characters."


-- Note: textField will someday use its string argument as greyed out
-- ghost text that is only visible when the textField is empty. Right
-- now the string is just ignored.