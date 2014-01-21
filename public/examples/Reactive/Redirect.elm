
import Graphics.Input as Input

main = flow down [ message, butn ]

(butn, pressed) = Input.button "Redirect to elm-lang.org"

port redirect : Signal String
port redirect =
    merge (constant "")
          (always "http://elm-lang.org" <~ pressed)

message = [markdown|

# Redirecting with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to redirect to a different page. The "redirect" port has a default
handler that reacts to non-empty strings.

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

|]

