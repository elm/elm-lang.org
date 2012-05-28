
component field txt =
  flow down
    [ field, text . monospace . toText $ "Your password is: " ++ txt ]

main = case Input.password "" of
       { (field, txt) -> lift (component field) txt }