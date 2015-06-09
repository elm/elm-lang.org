
# Interop

Interop is extremely important for any language that compiles to JavaScript. To be viable in a commercial setting, you need to be able to reuse helpful libraries and tools to get things done quickly. This section covers [how to embed Elm in HTML](#html-embedding) and [how to communicate with JavaScript](#ports).


## HTML Embedding

Elm can be embedded directly in a `<div>`. This lets you easily integrate Elm into a larger JS project. An embedded Elm program is called a &ldquo;component&rdquo;. This document shows how to embed Elm in HTML, and all of the following code [is available](https://gist.github.com/evancz/8456627).

Say you have a simple program [`Stamper.elm`](https://gist.github.com/evancz/8456627#file-stamper-elm) that lets you [stamp shapes by clicking](http://elm-lang.org/examples/Intermediate/Stamps.elm). Compiling it with:

```bash
elm-make Stamper.elm
```

will result in a file named `elm.js`. This JS file contains everything necessary for embedding in HTML.

In our HTML file [`EmbeddedElm.html`](https://gist.github.com/evancz/8456627#file-embeddedelm-html), we add a `<script>` below the `<body>` that includes the following code:

```javascript
// get an empty <div>
var div = document.getElementById('stamper');

// embed our Elm program in that <div>
Elm.embed(Elm.Stamper, div);
```

The `Elm.embed` function takes two arguments:

  1. An Elm module. All modules are prefixed with `Elm` in JavaScript to avoid namespace pollution, so our `Stamper` module becomes `Elm.Stamper`. 
  2. A `<div>` to embed the program in.

That's it!

Note that `Window.dimensions` and `Mouse.position` will be relative to the `<div>`, not the entire page. This means the `Stamper` code still fill the `<div>` entirely and handle clicks appropriately.

Now that you can embed an Elm program, learn how to communicate between Elm and JS with [ports](/learn/Ports.elm).

### Other ways to embed Elm

The example above embeds in a `<div>` but it is also possible to create Elm components that run fullscreen and ones that have no graphics at all. You generate the HTML automatically by specifying an HTML output file with `elm-make Stamper.elm --output=Main.html`

```javascript
// fullscreen version of Stamper
Elm.fullscreen(Elm.Stamper);

// Stamper with no graphics
Elm.worker(Elm.Stamper);
```

The version that has no graphics is useful for:

 * Writing logic in Elm but handling graphics with something else.
 * Using Elm with [node.js](http://nodejs.org/).

Furthermore, all of these Elm components can communicate with JavaScript via &ldquo;ports&rdquo;. [Read more about ports](/learn/Ports.elm) to see how to do this in more detail.


## Ports

Ports are a general purpose way to communicate with JavaScript. They let you send messages in and out of Elm so you can use JavaScript whenever you need to.

[This example](https://github.com/evancz/elm-html-and-js) and [this example](https://gist.github.com/evancz/8521339) show code you can take a look at, but this document will explain the specifics of what you can and cannot do with ports.


### From JavaScript to Elm

To send messages from JavaScript to Elm, you use an incoming port like this:

```haskell
port addUser : Signal (String, UserRecord)
```

This means we now have a signal in Elm called `addUser` that is updated by some code in JavaScript. To actually send messages to this port, we would write something like this in JavaScript:

```javascript
myapp.ports.addUser.send([
    "Tom",
    { age: 32, job: "lumberjack" }
]);

myapp.ports.addUser.send([
    "Sue",
    { age: 37, job: "accountant" }
]);
```

This sends two updates to Elm, automatically converting to values that work well in Elm.


### From Elm to JavaScript

To send messages from Elm to JavaScript, you define an outgoing port like this:

```haskell
port requestUser : Signal String
port requestUser =
    signalOfUsersWeWantMoreInfoOn
```

In this case we are taking a signal that exists in Elm and sending each of its values to JavaScript. On the JavaScript side, we handle these messages by subscribing to that port:

```javascript
myapp.ports.requestUser.subscribe(databaseLookup);

function databaseLookup(user) {
    var userInfo = database.lookup(user);
    myapp.ports.addUser.send(user, userInfo);
}
```

We have subscribed to the `requestUser` port and will actually go and do the database lookup. When we get the results, we send them back into Elm using another port. Perhaps at some point you will need to unsubscribe from a port, in which case you would do this:

```javascript
myapp.ports.requestUser.unsubscribe(databaseLookup);
```

Check out [this example](https://github.com/evancz/elm-html-and-js) or [this example](https://gist.github.com/evancz/8521339) to see these ideas in working code.


### Customs and Border Protection

Ports must be careful about what values are allowed through. Elm is statically typed, so each port is fitted with some border protection code that ensures that type errors are kept out. Ports also do some conversions so that you get nice colloquial data structures in both Elm and JS.

The particular types that can be sent in and out of ports is quite flexible, covering [all valid JSON values](http://www.json.org/). Specifically, incoming ports can handle all the following Elm types:

  * **Booleans and Strings** &ndash; both exist in Elm and JS!
  * **Numbers** &ndash; Elm ints and floats correspond to JS numbers
  * **Lists**   &ndash; correspond to JS arrays
  * **Arrays**  &ndash; correspond to JS arrays
  * **Tuples**  &ndash; correspond to fixed-length, mixed-type JS arrays
  * **Records** &ndash; correspond to JavaScript objects
  * **Signals** &ndash; correspond to event streams in JS
  * **Maybes**  &ndash; `Nothing` and `Just 42` correspond to `null` and `42` in JS
  * **Json**    &ndash; [`Json.Value`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json) corresponds to arbitrary JSON

All conversions are symmetric and type safe. If someone tries to give a badly typed value to Elm it will throw an error in JS immediately. By having a border check like this, Elm code can continue to guarantee that you will never have type errors at runtime.
