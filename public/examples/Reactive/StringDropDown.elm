
import Graphics.Input as Input

choices = [ "on a boat.", "with a goat.", "in the rain.",
            "on a train.", "here or there.", "ANYWHERE!" ]

(drop, choice) = Input.stringDropDown choices

component dropDown choice =
  let msg = "I do not like green eggs and ham. " ++
            "I will not eat them " ++ choice
  in  plainText msg `above` dropDown

main = lift2 component drop choice
