 
import Website.Skeleton (skeleton)
import Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "JavaScript Integration")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton intro) Window.width

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

<h1><div style="text-align:center">JavaScript Integration
<div style="font-size:0.5em;font-weight:normal">*Elm + JS*</div></div>
</h1>

Elm can be embedded directly in HTML or JS. This means Elm
integrates with your existing workflow, whether you make web apps
or work with server-side JS. Using Elm is not an all-or-nothing choice.

This lowers the barrier if you want to experiment with Elm and makes
it easier to convince your boss that it is okay to use
Elm in an existing project.

This post will cover:

* Creating Elm programs that are fullscreen, in a `<div>`, or just a worker
* Communicating between Elm and JS

It should be possible to call Elm functions directly from JS at some point, but
that API is not ready yet.

## Embedding Elm in HTML

The following video walk-through is a basic example
of embedding Elm in HTML. The code is available to download
and play around with [here](https://gist.github.com/evancz/5581910).

<div style="position:relative; height:350px;">
<iframe width="600" height="350"
        src="http://www.youtube.com/embed/xt07tLqa_m8?rel=0"
             style="position:absolute; margin-left:-300px; left:50%;"
        frameborder="0" allowfullscreen></iframe>
</div>

Okay, so now that we have seen what is possible, let&rsquo;s see the
APIs in detail.

## Elm modules in JavaScript

We will start with a Hello World module which simply displays the
text &ldquo;Hello, World!&rdquo;:

```haskell
module HelloWorld where

main = plainText "Hello, World!"
```

Elm modules are attached to the global `Elm` object in JavaScript,
so to refer to this module we say `Elm.HelloWorld`.
We can initialize this module in three different ways:

```javascript
// Take over the <body>
var elm1 = Elm.fullscreen(Elm.HelloWorld);

// Take over <div id="hello-elm"></div>
var elm2 = Elm.byId('hello-elm', Elm.HelloWorld);

// Start an Elm worker with no graphics
var elm3 = Elm.worker(Elm.HelloWorld);
```

When you use `Elm.byId`, `Window.dimensions` and `Mouse.position` will be
relative to the `<div>`, not the entire page.

Notice that we created three objects: `elm1`, `elm2`, and `elm3`. These objects
give access to the `send` and `recv` functions which let you communicate with
Elm. The next section explains these functions in more detail.

## Communication between Elm and JS

Say we wrote a module called `WorkHorse` that does a bunch of computation in Elm.
We would like to send events from JS through `WorkHorse` for processing.

#### JS to Elm

First we initialize our module without any of the rendering tools:

```javascript
var workhorse = Elm.worker(Elm.WorkHorse);
```

The `workhorse` object has two methods, `send` and `recv`, which do exactly as
they suggest: you send messages to Elm with `send` and receive messages
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
in callback heavy code. That means that the following two definitions are
equivalent:

```javascript
var sendVerbose =
        function(v) { elm.send('do-something-in-elm', v); };
var sendCurried = elm.send('do-something-in-elm');
```

Both functions will send a message to Elm: `sendVerbose(42)`
and `sendCurried(42)`.

Great, we can send and receive messages in JavaScript, but how do we
do the same in Elm?

#### Elm to JS

Elm talks to JavaScript by piping signals in and out of a program.
A signal update translates into an event in JavaScript. So say we
are actually writing the `WorkHorse` module now, and we want to
to communicate on the `"do-something-in-elm"` and `"do-the-rest-in-js"`
channels we discussed above:

```haskell
module WorkHorse where

import JavaScript as JS

foreign import jsevent "do-something-in-elm"
    (JS.fromString "")
    computationRequests : Signal JS.JSString

results = doSomeStuff computationRequests

foreign export jsevent "do-the-rest-in-js"
    results : Signal JS.JSString
```

So the two key parts of this are the `foreign import` and `foreign export`.
The import is bringing in events from JavaScript. It needs a base case because
signals must always have a current value. If in JavaScript, we say:

```javascript
workhorse.send('do-something-in-elm', 'Hello, WorkHorse!');
```

The value of `computationRequests` will update to `"Hello, WorkHorse!"`. That will
trigger the `doSomeStuff` function and finally flow through to the export. The
foreign export sends the result back to JavaScript, triggering any function that is
receiving on the `"do-the-rest-in-js"` channel.

There are some restrictions on what can be passed between Elm and JS. The values you
can send:

* **Must be native JavaScript values.**
  This is so that implementation details do not leak out and become
  &ldquo;features&rdquo;. This gives Elm the freedom to drastically
  change its internals if a good reason arises (such as the major performance
  improvements that came with version 0.8).
* **Cannot be functions.** Elm only permits
  [pure functions](http://en.wikipedia.org/wiki/Pure_function) which makes code
  easier to understand in general and is particularly important for concurrency.
  This restriction on function imports makes it
  impossible to sneak impure functions into Elm. It also allows Elm to easily
  use more efficient calling conventions internally.

The standard term for this kind of thing is a Foreign Function Interface (FFI),
but it is probably more precise to say that it is a Foreign Event
Interface since you cannot actually share functions with this API.

## Converting between Elm and JavaScript values

Elm provides the [`JavaScript`](/docs/JavaScript.elm) and
[`Json`](/docs/Json.elm) libraries to convert between Elm and
JS values. You will need to use this library to
work with JS values in Elm or to send values to JS.

This layer of abstraction decouples
Elm from any of the particulars of its implementation. Long term, this
will permit the compiler and runtime system to become faster and more
clever without any changes to Elm itself.

## Example: Setting Cookies

In this example, we want to set some cookies. This is currently not possible
directly in Elm, so we can do some of this work in JavaScript.

Our example program is a painting application, and we want to save the paint
brush color accross sessions.

```haskell
module Paint where

import JavaScript as JS

brushColor = -- some signal

cookies = lift (JS.fromString . show) brushColor

foreign export jsevent "set-brush-color-cookie"
  cookies : Signal JS.JSString
```
So we start with some code that exports values to JS. In JavaScript,
we receive these values with `recv` and actually set the cookie:

```javascript
elm.recv( 'set-brush-color-cookie', function(event) {
        createCookie( 'brush-color', event.value, 5 );
    });

function createCookie( name, value, days ) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
    document.cookie = name+"="+value+expires+"; path=/";
}
```

So we are now persisting the brush color for five days, allowing the user
to come back to their painting app and find their settings have been preserved!

|]
