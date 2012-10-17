
{------------------------------------------------------------------

  This example exports a jsevent called "elm_redirect" which
  built-in to Elm. You can use this event without writing any
  JavaScript.

------------------------------------------------------------------}


module Redirect where

import JavaScript
import Signal.Input


(butn, pressed) = button " Redirect to elm-lang.org "

redirectTo =
  lift castStringToJSString $
  keepWhen pressed "" (constant "http://elm-lang.org/")

foreign export jsevent "elm_redirect"
  redirectTo :: Signal JSString

main = butn