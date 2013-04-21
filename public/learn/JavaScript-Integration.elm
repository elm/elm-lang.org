
import Website.Skeleton
import Website.ColorScheme
import Window as Window
import JavaScript as JS

title = constant (JS.fromString "JavaScript Integration")
foreign export jsevent "elm_title"
  title : Signal JSString

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

Elm&rsquo;s JavaScript integration bridges the gap between the two languages
without sacrificing the key benefits of Elm. You do not have to give up strong
static typing or the module system to get some of the benefits of JS. We will
go through:

* Initialize Elm: full-page, in a DOM node, without graphics
* Communicate with Elm

It should be possible to call Elm functions directly from JS at some point, but
that API is not ready yet.

## Elm modules in JavaScript

Modules are the basic unit of compilation in Elm, and
any module can be initialized from JavaScript. You can
have modules take over the whole window, squeeze themselves into a `<div>`,
or even turn the renderer off altogether and just run computations. Let&rsquo;s
look at these three possibilities in action!

We will start with a Hello World module which simply displays the
text &ldquo;Hello, World!&rdquo;:

```haskell
module HelloWorld where

main = plainText "Hello, World!"
```

Elm modules are attached to the global `Elm` object in JavaScript.
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

In this case, `Window.dimensions` and `Mouse.position` are given relative to the
actual Elm `<div>`, not the entire page.

Finally, you can start an Elm module without any graphics at all. The primary
usage of this would be in a purely JS environment such as node.js.

```javascript
var computeElm = Elm.init(Elm.HelloWorld, null);
```

Next we will see how to communicate between Elm and JS. This makes it possible
to use Elm and JS together, using each for what they do best.

## Communication between Elm and JS

Say we wrote a module called `WorkHorse` that does a bunch of computation in Elm.
We would like to send events from JS through `WorkHorse` for processing.

#### JS to Elm

First we initialize our module without any of the rendering tools:

```javascript
var workhorse = Elm.init(Elm.WorkHorse, null);
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
    computationRequests : Signal JSString

results = doSomeStuff computationRequests

foreign export jsevent "do-the-rest-in-js"
    results : Signal JSString
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
* **Cannot be functions.** This is to make it impossible to sneak impure
  functions into Elm, and to allow Elm to use more efficient calling
  conventions internally.

The standard term for this kind of thing is a Foreign Function Interface (FFI),
but it is probably more precise to say that it is a Foreign Event
Interface since you cannot actually share functions with this API.

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
  cookies : Signal JSString
```
So we start with some code that exports values to JS. In JavaScript,
we receive these values with `recv` and actually set the cookie:

```javascript
elm.recv('set-brush-color-cookie', function(event) {
        createCookie('brush-color', event.value, 5);
    });

function createCookie(name,value,days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
    document.cookie = name+"="+value+expires+"; path=/";
}
```

So we are now persisting the brush color for five days, allowing the user
to come back to their painting app and find their settings have been preserved!

|]
