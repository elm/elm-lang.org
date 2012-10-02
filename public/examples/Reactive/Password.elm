
import Signal.Input (password)

component field txt =
  field `above` (text . monospace . toText $ "Your password is: " ++ txt)

main = let (field, txt) = password "Password" in
       lift (component field) txt