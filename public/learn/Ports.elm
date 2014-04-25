
import Website.Skeleton (skeleton)
import Website.ColorScheme
import Window

port title : String
port title = "Ports"

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

# Ports: Communicate with JS

This idea comes from a “component model” for using Elm in production systems.
Realistically, folks are not going to go all Elm all at once. A component model
means you write small UI widgets or signal processing units in Elm and [embed them
in a larger system](/learn/Components.elm) that uses lots of different things.
Ports are a nice way of making these components simple and composable.

[This example](https://github.com/evancz/elm-html-and-js) and
[this example](https://gist.github.com/evancz/8521339) show
code you can take a look at, but this document will explain
the specifics of what you can and cannot do with ports.

## Ports in Elm

Ports are unified way to send information to and from an Elm program. Ports are
geared towards communicating via signals, but they also allow non-signal values
through. For example:

```haskell
module Chat where

-- incoming messages typed by your chat parter
port messageIn : Signal String

myTextInput = ...

-- outgoing messages typed in by you
port messageOut : Signal String
port messageOut = myTextInput
```

This will create two ports named `messageIn` and `messageOut`.
If the port has no definition, it must be an incoming port. This means
`messageIn` is a signal coming into the Elm program from JavaScript. If
a port *does* have a definition it is an outgoing port. That means `messageOut`
is sending the values of `myTextInput` out to JavaScript.

## Initializing a component with ports

You should already know [how to initialize an Elm module in JS](/learn/Components.elm).
When a module has incoming ports, you must provide a value for each one:

```javascript
var chat = Elm.embed(Elm.Chat, div, { messagesIn: "" });
```

The last argument to `Elm.embed` is an object that has a field for each incoming port.
It works the same for `Elm.fullscreen` and `Elm.worker`.

If you do not provide every port declared in the module, it will throw a JS error.
If you provide *extra* ports, it will throw a JS error. Each port also has a
known type in Elm, so Elm will perform validation on incoming values and throw
an error if a value of the wrong type is given.

Notice that when you initialize an Elm module, you get an object back.
The most important field in this object is `chat.ports` which contains
all of the incoming and outgoing ports that you may need to interact with.

#### Sending messages to Elm

```javascript
chat.ports.messageIn.send("hey");
chat.ports.messageIn.send("what's up?");
```

These incoming signals are typed, so a JS error will be thrown if you provide
an invalid value.

#### Receiving messages from Elm

```javascript
function logger(x) { console.log(x); }

// attach a logger, printing all outgoing messages to console
chat.ports.messageOut.subscribe(logger);

// detach the logger
chat.ports.messageOut.unsubscribe(logger);
```

You may subscribe many different functions. Each will be called
once for every outgoing event.

## Customs and Border Protection

Ports must be careful about what values are allowed through.
Elm is statically typed, so each port is fitted with some
border protection code that ensures that type errors are kept
out. Ports also do some conversions so that you get nice
colloquial data structures in both Elm and JS.

The particular types that can be sent in and out of ports is
actually quite flexible. It covers pretty much [all valid JSON
values](http://www.json.org/). Incoming ports can handle [JS
values](http://library.elm-lang.org/catalog/evancz-Elm/0.12/JavaScript)
and the following Elm types:

  * **Booleans and Strings** &ndash; both exist in Elm and JS!
  * **Numbers** &ndash; Elm ints and floats correspond to JS numbers
  * **Lists**   &ndash; correspond to JS arrays
  * **Tuples**  &ndash; correspond to fixed-length, mixed-type JS arrays
  * **Records** &ndash; correspond to JavaScript objects
  * **Signals** &ndash; correspond to event streams in JS
  * **Maybes**  &ndash; `Nothing` and `Just 42` correspond to `null` and `42` in JS

All conversions are symmetric and type safe. If someone tries to give a
badly typed value to Elm it will throw an error in JS immediately. By having
a border check like this, Elm code can continue to guarantee that you will
never have type errors at runtime.

Outgoing ports let you export all of the values listed above with
one important addition: first-order functions!
If you wrote a nice parser or library in Elm, you can use those functions
directly in JS. The mapping between Elm and JS function looks like this:

    add x y = x + y

    function add(x,y) { return x + y; }

You lose currying on the JS side, but the goal of this whole feature is to
produce *colloquial* values in both Elm and JS. One important restriction on
exporting functions is that they must be *first-order* functions. Things
like `map` and `foldl` cannot be exported because the Elm compiler may
eventually perform optimizations that assume purity, and higher-order
functions allow you to introduce impure functions which *could* be executed
in an unexpected order.

|]
