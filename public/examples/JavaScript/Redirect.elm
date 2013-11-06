-- This example exports a built-in jsevent called "redirect"

import JavaScript as JS
import Graphics.Input as Input

main = butn

(butn, pressed) = Input.button "Redirect to elm-lang.org"

redirectTo =
    JS.fromString <~ merges [ constant ""
                            , (\_ -> "http://elm-lang.org/") <~ pressed
                            ]

foreign export jsevent "redirect"
  redirectTo : Signal JS.JSString

