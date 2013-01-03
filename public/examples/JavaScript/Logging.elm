
{------------------------------------------------------------------

  You must open the 'Developer Console' of your browser to see
  the results of this example. Just press "F12" in most browsers.

  This example exports a jsevent called "elm_log" which built-in
  to Elm. You can use this event without writing any JavaScript.

------------------------------------------------------------------}


module Logging where

import JavaScript
import Input


(field, message) = textField ""
(butn , pressed) = button " Log "


messages =
  castStringToJSString <~ keepWhen pressed "" message

foreign export jsevent "elm_log"
  messages :: Signal JSString


main = field `beside` butn