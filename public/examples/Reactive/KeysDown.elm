
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

import Signal.Keyboard.Raw (keysDown)

display codes =
  plainText "You are holding down the following keys: " `beside` asText codes


main = lift display keysDown