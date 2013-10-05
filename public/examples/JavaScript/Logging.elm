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

messages = JS.fromString <~ message
foreign export jsevent "log"
  messages : Signal JS.JSString
