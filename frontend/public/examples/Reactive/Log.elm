import Markdown
import Time (..)


port log : Signal Float
port log = every second


main = Markdown.toElement """

# Logging with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to log to the developer console. The "log" port has a default
handler that will call `console.log` on whatever it receives.

Just open the 'Developer Console' to see the logged messages!

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

"""