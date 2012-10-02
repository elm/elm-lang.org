
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Data.Char (fromCode)
import Signal.Keyboard.Raw

latestKey = let step curr prev = maybe prev fromCode curr in
            foldp step '_' charPressed 

display chr =
  text $ toText "The last key you pressed was: " ++ show chr

main = lift display latestKey