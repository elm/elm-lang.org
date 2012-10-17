
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Char (fromCode)
import Keyboard.Raw

latestKey = let step curr prev = maybe prev fromCode curr in
            foldp step '_' charPressed 

display char =
  plainText "The last key you pressed was: " `beside` asText char

main = lift display latestKey