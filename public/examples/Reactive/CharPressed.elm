
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

latestKey = let step curr prev = maybe prev Char.fromCode curr in
            foldp step '_' Keyboard.Raw.charPressed 

display char =
  plainText "The last key you pressed was: " `beside` asText char

main = lift display latestKey