
component field txt =
  field `above` (text . monospace . toText $ "Your password is: " ++ txt)

(field, txt) = Input.password "Password"

main = component field <~ txt