
{------------------------------------------------------------------

  Compile to "New Tab" in the bottow right corner to see this
  code in action.

  This example exports a jsevent called "elm_title" which
  built-in to Elm. You can use this event without writing any
  JavaScript.

------------------------------------------------------------------}


module ChangeTitle where

import JavaScript
import Input


(field, title) = let (f,t) = textField "" in
                 (f, castStringToJSString <~ t)

foreign export jsevent "elm_title"
  title :: Signal JSString

main = plainText "Change this page's title to: " `beside` field
