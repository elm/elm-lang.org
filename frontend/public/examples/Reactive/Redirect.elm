import Graphics.Element exposing (..)
import Graphics.Input as Input
import Markdown
import Signal


main : Element
main =
  flow down
    [ message
    , Input.button (Signal.send click ()) "Redirect to elm-lang.org"
    ]


click : Signal.Channel ()
click =
  Signal.channel ()


port redirect : Signal String
port redirect =
  Signal.merge
    (Signal.constant "")
    (Signal.map (always "http://elm-lang.org") (Signal.subscribe click))


message : Element
message = Markdown.toElement """

# Redirecting with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to redirect to a different page. The "redirect" port has a default
handler that reacts to non-empty strings.

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

"""