{------------------------------------------------------------------
  Follow this link to see the title change:
  

  This example exports a jsevent called "title" which is
  built-in and operates on a per-module basis.
------------------------------------------------------------------}

import JavaScript as JS
import Graphics.Input as Input

(field, input) = Input.field ""

title = JS.fromString <~ input
foreign export jsevent "elm_title"
  title : Signal JS.JSString

scene fld =
    plainText "Change this page's title to: " `beside` fld

main = scene <~ field
