
import Graphics.Input as Input

main : Element
main = flow down [ message
                 , Input.button portal () "Redirect to elm-lang.org" ]

(clicks, portal) = Input.input ()

port redirect : Signal String
port redirect =
    merge (constant "")
          (always "http://elm-lang.org" <~ clicks)

message : Element
message = [markdown|

# Redirecting with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to redirect to a different page. The "redirect" port has a default
handler that reacts to non-empty strings.

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

|]

