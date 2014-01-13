-- This example exports a built-in jsevent called "redirect"

import JavaScript as JS
import Graphics.Input as Input

main = butn

(butn, pressed) = Input.button "Redirect to elm-lang.org"

port redirect : Signal String
port redirect =
    merge (constant "")
          ((\_ -> "http://elm-lang.org/") <~ pressed)

