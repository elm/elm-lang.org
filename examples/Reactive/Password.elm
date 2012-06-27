
import Signal.Input (password)

component field txt =
  field `above` (text . monospace . toText $ "Your password is: " ++ txt)

main = let (field, txt) = password "" in
       lift (component field) txt