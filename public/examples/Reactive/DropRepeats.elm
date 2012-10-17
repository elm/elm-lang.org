
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Char (fromCode)
import Keyboard.Raw

latestKey = let step curr prev = maybe prev fromCode curr in
            foldp step '_' charPressed 

display chr =
  plainText $ "Number of non-repeated key presses: " ++ show chr

main = lift display $ count (dropRepeats latestKey)