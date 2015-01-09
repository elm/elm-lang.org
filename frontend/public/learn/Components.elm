import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Website.ColorScheme
import Window

port title : String
port title = "Embed Elm in HTML"


main : Signal Element
main =
  skeleton "Learn" (\w -> width (min 600 w) intro) <~ Window.dimensions


intro : Element
intro = Markdown.toElement """

# Components: Embed in HTML

Elm can be embedded directly in a `<div>`. This lets you easily integrate
Elm into a larger JS project. An embedded Elm program is called a
&ldquo;component&rdquo;. This document shows how to embed Elm in HTML, and
all of the following code [is available](https://gist.github.com/evancz/8456627).

Say you have a simple program
[`Stamper.elm`](https://gist.github.com/evancz/8456627#file-stamper-elm)
that lets you [stamp shapes by
clicking](http://elm-lang.org/examples/Intermediate/Stamps.elm).
Compiling it with

    elm-make Stamper.elm

will result in a file named `elm.js`. This JS file contains
everything necessary for embedding in HTML.

In our HTML file [`EmbeddedElm.html`](https://gist.github.com/evancz/8456627#file-embeddedelm-html),
we add a `<script>` below the `<body>` that includes the following code:

```javascript
// get an empty <div>
var div = document.getElementById('stamper');

// embed our Elm program in that <div>
Elm.embed(Elm.Stamper, div);
```

The `Elm.embed` function takes two arguments:

  1. An Elm module. All modules are prefixed with `Elm` in JavaScript to avoid
     namespace pollution, so our `Stamper` module becomes `Elm.Stamper`. 
  2. A `<div>` to embed the program in.

That's it!

Note that `Window.dimensions` and `Mouse.position` will be
relative to the `<div>`, not the entire page. This means the
`Stamper` code still fill the `<div>` entirely and handle
clicks appropriately.

Now that you can embed an Elm program, learn how to communicate
between Elm and JS with [ports](/learn/Ports.elm).

## Other ways to embed Elm

The example above embeds in a `<div>` but it is also possible to
create Elm components that run fullscreen and ones that have no
graphics at all.
You generate the HTML automatically by specifying an HTML output file with
`elm-make Stamper.elm --output=Main.html

```javascript
// fullscreen version of Stamper
Elm.fullscreen(Elm.Stamper);

// Stamper with no graphics
Elm.worker(Elm.Stamper);
```

The version that has no graphics is useful for:

 * Writing logic in Elm but handling graphics with something else.
 * Using Elm with [node.js](http://nodejs.org/).

Furthermore, all of these Elm components can communicate with JavaScript
via &ldquo;ports&rdquo;. [Read more about ports](/learn/Ports.elm) to see
how to do this in more detail.

"""
