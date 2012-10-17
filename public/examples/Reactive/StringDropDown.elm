
import Input (stringDropDown)

component dropDown choice =
  let msg = "I do not like green eggs and ham. " ++
            "I will not eat them " ++ choice
  in  plainText msg `above` dropDown

choices = [ "on a boat."
          , "with a goat."
          , "in the rain."
          , "on a train."
          , "here or there."
          , "ANYWHERE!"
          ]

main = let (drop, choice) = stringDropDown choices in
       lift (component drop) choice