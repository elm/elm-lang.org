{------------------------------------------------------------------
  You must open the 'Developer Console' of your browser to see
  the results of this example.

  This example exports a built-in jsevent called "log" which writes
  strings to the developer console.
------------------------------------------------------------------}

import JavaScript as JS
import Graphics.Input as Input

main = field

(field, message) = Input.field ""

port log : Signal String
port log = message
