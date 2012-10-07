
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Signal.Keyboard.Raw (keysDown)

display codes =
  let keys = monospace . toText $ show codes in
  text $ toText "You are holding down the following keys: " ++ keys
                

main = lift display keysDown