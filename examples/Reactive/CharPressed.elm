
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Data.Char (fromCode)

update chr oldChr =
  case chr of { Just c -> fromCode c ; Nothing -> oldChr }
latestKey = foldp update '_' Keyboard.Raw.charPressed 

display chr =
  text $ toText "The last key you pressed was: " ++ show chr

main = lift display latestKey