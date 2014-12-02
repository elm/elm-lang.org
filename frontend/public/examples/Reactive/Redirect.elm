
import Graphics.Input as Input

main : Element
main =
    flow down
        [ message
        , Input.button click.handle () "Redirect to elm-lang.org"
        ]

click : Input.Input ()
click = Input.input ()

port redirect : Signal String
port redirect =
    merge
        (constant "")
        (always "http://elm-lang.org" <~ click.signal)

message : Element
message = [markdown|

# Redirecting with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to redirect to a different page. The "redirect" port has a default
handler that reacts to non-empty strings.

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

|]

