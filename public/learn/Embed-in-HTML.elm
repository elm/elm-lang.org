
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
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
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
