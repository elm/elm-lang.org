{------------------------------------------------------------------
  Follow this link to see the title change:
  

  This example exports a jsevent called "title" which is
  built-in and operates on a per-module basis.
------------------------------------------------------------------}

import JavaScript as JS
import Graphics.Input as Input

(field, input) = Input.field ""

port title : Signal String
port title = input

scene fld =
    plainText "Change this page's title to: " `beside` fld

main = scene <~ field
