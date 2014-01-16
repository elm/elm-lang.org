
import Website.Skeleton (skeleton)
import Website.ColorScheme
import Window
import JavaScript as JS

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

# Ports: Communicate with JS

This idea comes from a “component model” for using Elm in production systems.
Realistically, folks are not going to go all Elm all at once. A component model
means you write small UI widgets or signal processing units in Elm and [embed them
in a larger system](/learn/Embde-in-HTML.elm) that uses lots of different things.
Ports are a nice way of making these components simple and composable.

## Ports in Elm

Ports are unified way to send information to and from an Elm program. Ports are
geared towards communicating via signals, but the also allow non-signal values
through. For example:

```haskell
module Chat where

-- Bring in a signals of messages typed by your chat parter
port messageIn : Signal String

myTextInput = ...

-- Send out a signal of messages typed in by you
port messageOut : Signal String
port messageOut = myTextInput
```

This will create two ports named `messageIn` and `messageOut`.
If the port has no definition, it must be an incoming port. This means
`messageIn` is a signal coming into the Elm program from JavaScript. If
a port *does* have a definition it is an outgoing port. That means `messageOut`
is sending the values of `myTextInput` out to JavaScript.

## Initializing a component with ports

You should already know [how to initialize an Elm module in JS](/learn/Embed-in-HTML.elm).
When a module has incoming ports, you must provide a value for each one:

```javascript
var chat = Elm.embed(Elm.Chat, div, { messagesIn: "" });
```

The last argument to `Elm.embed` is an object that has a field for each incoming port.
If you do not provide all ports declared in the module, it will throw an error. If you
provide extra ports, it will throw an error. Each port also has a known type in Elm,
so Elm will perform validation on incoming values and throw an error if a value of the
wrong type is given.

If you have a bunch of ports coming in, it’d look more like this:

```javascript
var chat = Elm.embed(Elm.Chat, div, {
    messagesIn: "",
    userID: 12345,
    userName: "Tom"
});
```

Notice that when you initialize an Elm module, you get an object back.
The most important field in this object is `chat.ports` which contains
all of the incoming and outgoing ports that you may need to interact with.

## Sending messages to Elm

```javascript
chat.ports.messageIn.send("hey, what's up?");
```

## Receiving messages from Elm

```javascript
function logger(x) { console.log(x); }

// attach a logger, printing all outgoing messages to console
chat.ports.messageOut.subscribe(logger);

// detach the logger
chat.ports.messageOut.unsubscribe(logger);
```

|]
