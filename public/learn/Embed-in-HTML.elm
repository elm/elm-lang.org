
import Website.Skeleton (skeleton)
import Website.ColorScheme
import Window
import JavaScript as JS

port title : String
port title = "Embed Elm in HTML"

main = lift (skeleton intro) Window.dimensions

intro w = width (min 600 w) [markdown|

<style type="text/css">
p { text-align: justify }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: #f8f8f8; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: #f8f8f8; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

# How to embed Elm in HTML

Elm can be embedded directly in a `<div>`. This lets you easily integrate
Elm into a larger JS project. This page tells you how. All of the following
code is available [here](https://gist.github.com/evancz/8456627).

Say you have a simple program [`Stamper.elm`](https://gist.github.com/evancz/8456627#file-stamper-elm)
that lets you [stamp shapes by clicking](http://elm-lang.org/examples/Intermediate/Stamps.elm).
Compiling it with

    elm --only-js Stamper.elm

will result in a file named `build/Stamper.js`. This JS file contains
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

## Additional Facts

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

```javascript
// fullscreen version of Stamper
Elm.fullscreen(Elm.Stamper);

// Stamper with no graphics
Elm.worker(Elm.Stamper);
```

The version that has no graphics is useful for:

 * Writing logic in Elm but handling graphics with something else.
 * Using Elm with [node.js](http://nodejs.org/).

Read more about [ports](/learn/Ports.elm) to see how to do this in
more detail.

|]
