
component dropDown choice =
  flow down [ plainText $ "I do not like green eggs and ham. " ++
                          "I will not eat them " ++ choice
            , dropDown ]

choices = [ "on a boat."
          , "with a goat."
          , "in the rain."
          , "on a train."
          , "here or there."
          , "ANYWHERE!"
          ]

main = case Input.stringDropDown choices of
       { (drop, choice) -> lift (component drop) choice }