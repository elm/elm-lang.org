
port title : Signal String
port title = show <~ every second

main = [markdown|

# Setting titles with ports

This example uses [ports](http://elm-lang.org/learn/Ports.elm)
to set the pages title. See the title changing live
<a href="/examples/Reactive/Title.elm" target="_top">here</a>.

See [the full list of built-in port
handlers](/learn/Syntax.elm#javascript-ffi) for more information.

|]