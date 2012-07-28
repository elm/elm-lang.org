
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Signal.Keyboard.Raw (keysDown)

display codes =
  text $ toText "You are holding down the following keys: " ++ show codes

main = lift display keysDown