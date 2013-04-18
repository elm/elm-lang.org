
import Website.Skeleton
import Website.ColorScheme
import Window as Window
import JavaScript as JS

title = constant (JS.fromString "JavaScript Integration")
foreign export jsevent "elm_title"
  title : Signal JSString

main = lift (skeleton intro) Window.width

intro w = width w [markdown|

<style type="text/css">
p { text-align: justify }
h2,h3,h4 { padding-top: 0.5em; }
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

<h1><div style="text-align:center">JavaScript Integration
<div style="font-size:0.5em;font-weight:normal">*Elm + JS*</div></div>
</h1>

Elm&rsquo;s JavaScript integration bridges the gap between the two languages
without sacrificing the key benefits of Elm. You do not have to give up strong
static typing or the module system to get some of the benefits of JS. We will
go through:

* Elm in the `<body>`
* Elm in *any* DOM node
* Embedding DOM nodes in Elm
* Elm without the renderer, just for computation
* Sending events to and from Elm

It should be possible to call Elm functions directly from JS at some point, but
that API is not ready yet.

## Initializing an Elm module in JavaScript

Elm allows you to start many copies of a module from JavaScript. You can
have these modules take over the screen, squeeze themselves into a `<div>`,
or even turn the renderer off altogether and just run computations.

We will start with a Hello World module:

```haskell
module HelloWorld where

main = plainText "Hello, World!"
```

All Elm modules are attached to the global `Elm` object.
We can initialize each module with the `Elm.init` function.
In our case we could say:

```javascript
var fullScreenElm = Elm.init(Elm.HelloWorld);
```

This takes over the whole `<body>`, making it full screen. That is all you
need to do to start a normal Elm program, and in most cases the compiler will set
this up for you.

You can also embed an Elm module in a particular `<div>`. This means
you can easily bring a small Elm component into an existing JavaScript
project.

```javascript
var node = document.createElement('div');
var embeddedElm = Elm.init(Elm.HelloWorld, node);
```

Finally, you can start an Elm module without any graphics at all. The primary
usage of this would be in a purely JS environment such as node.js.

```javascript
var computeElm = Elm.init(Elm.HelloWorld, null);
```

Next we will see how to communicate between Elm and JS. This makes it possible
to use Elm and JS together, using each for what they do best.

## Communicating with Elm

Say we wrote a module called `WorkHorse` that does a bunch of computation in Elm.
We would like to send events from JS through `WorkHorse` for processing.

First we initialize our module without any of the rendering tools:

```javascript
var workhorse = Elm.init(Elm.WorkHorse, null);
```

The `workhorse` object has two methods, `send` and `recv`, which do exactly as
they suggest. You can send messages to Elm with `send` and receive messages
from Elm with `recv`.

Both `send` and `recv` work with labeled events, allowing many channels of
communication between Elm and JS. Example uses would look like this:

```javascript
workhorse.send('do-something-in-elm', payload);
workhorse.recv('do-the-rest-in-js', function(valueFromElm) {
    // whatever else needs to happen
  });
```

In the first line, we just send a `payload` to Elm.
In the second line, we receive some `valueFromElm` which we can then use
however we would like.

The `elm.send` function can also be curried, which may make it easier to use
in callback heavy code. That means that the following two statements are
equivalent:

```javascript
var sendVerbose = function(v) { elm.send('do-something-in-elm', v); }
var sendCurried = elm.send('do-something-in-elm')
```

## Small Example

Stuff in Elm

```haskell
module WorkHorse where



import JavaScript as JS

foreign export jsevent "computation_request"
  requests : Signal JSString

foreign import jsevent "computation_result"
  (JS.fromString "")
  results : Signal JSString
```
in JS

```javascript
function jsComputation(value) {
    var out = doSomethingExpensive(value);
    elm.send('computation_result', out);
}

elm.recv('computation_request', jsComputation);
```



|]
