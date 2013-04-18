
import Website.Skeleton
import Website.ColorScheme
import Window as Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.8")
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

<h1><div style="text-align:center">Elm 0.8
<div style="font-size:0.5em;font-weight:normal">*Faster and more useful*</div></div>
</h1>

A new release.

* Type annotations and type aliases
* More efficient function calls and ADT representation
* Improve the `collage` API
* Allow dynamic creation of GUI inputs
* Better JS integration

## Initializing an Elm module in JavaScript

All Elm modules are attached to the global `Elm` object in JavaScript.
We can initialize arbitrarily many independent versions of each module
with the `Elm.init` function. The most basic use is as follows:

```javascript
var elm = Elm.init(Elm.Main);
```

This takes over the whole body of the document, which is how Elm has worked
up until now. There are two other ways to initialize an Elm module though:

```javascript
// Put the Elm program in a normal DOM node.
var node = document.createElement('div');
var elm2 = Elm.init(Elm.Main, node);

// Turn off the renderer and graphics entirely.
// Only use Elm for computation and processing events.
var elm3 = Elm.init(Elm.Main, null);
```

These cover two new use cases: embedding Elm in an existing project and embedding
Elm in a purely JS environment such as node.js.

## Communicating with Elm

Say we only want to use Elm for processing events. We would initialize our module
without any of the rendering tools:

```javascript
var elm = Elm.init(Elm.Main, null);
```

The `elm` object holds two functions which make it easier to communicate
between the Elm module and JavaScript. 

* `send` which takes an event name and a value. It sends these into the Elm
  program.
* `recv` which takes an event name and a handler function. It receives values
  from Elm and processes them with the specified handler.

```javascript
elm.send('do-something-in-elm', payload);
elm.recv('do-the-rest-in-js', function(value) {
    // whatever else needs to happen
  });
```

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
module Main where

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
