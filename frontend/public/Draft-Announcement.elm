import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Window


main : Html
main =
  div [ myStyle ] [ content ]


myStyle : Attribute
myStyle =
  style
    [ ( "width", "600px" )
    , ( "display", "block" )
    , ( "margin-left", "auto" )
    , ( "margin-right", "auto" )
    ]


words = """
Elm has always been guided by the idea that a language should have a very
strict budget for complexity. If I have to spend part of my complexity budget,
I better be getting a lot of great *practical* benefits.

This has served us extraordinarily well, keeping the language simple *and*
allowing us to create shockingly powerful debugging tools.
"""


content : Html
content = Markdown.toHtml """

<h1><div style="text-align:center">Elm 0.15
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">Promises</div></div>
</h1>

<div style="color:red; text-align:center;">THIS IS A DRAFT, NOT INDENDED FOR DISTRIBUTION!</div>

Elm now has support for promises. A `Promise` is a promise to run some
computation *later*. Let&rsquo;s look at an example in which we want to fetch
the coolest hats in our store. We will start with a promise to just get all the
hats:

```elm
getHats : Promise Http.Error (List Hat)
```

The value `getHats` does not talk to a server, it describes *how* to talk to
the server. When it is run at some point in the future, it can have two
different outcomes:

  1. Something goes wrong talking to the server, resulting in an `Http.Error`
  2. Everything goes according to plan and we get a list of hats!

The most important part of promises is that they can be chained with the
`andThen` function. This means that when a promise is successful, we can use
the resulting value to do more stuff.

```elm
andThen : Promise x a -> (a -> Promise x b) -> Promise x b

succeed : a -> Promise x a

getCoolestHats : Promise Http.Error (List Hat)
getCoolestHats =
    getHats `andThen` \\hats ->
        succeed (List.sortBy .coolness hats)
```

In the `getCoolestHats` function we use `andThen` to take the results of
`getHats` and then sort them by coolness. We can keep chaining promises
together as long as we want, fetching information on shoes and jackets and
pants to try to create the coolest outfit.

To take a step back, promises give Elm the ability to represent *any*
low-level browser API in a safe and principled way. This means we can start
talking about local storage, audio, web sockets, geolocation, etc. in Elm *and*
keep great features like time-travel debugging. So this is a very important
release for Elm, and the addition of promises guides nearly all the following
changes.


### Inputs / Outputs

The syntax for ports has been revised to fit promises in a clean way. Incoming
and outgoing ports now have their own keywords: `input` and `output`. The
following example modernizes parts of [this program][port]:

  [port]: https://gist.github.com/evancz/8521339

```elm
input incomingShip : Stream { name : String, capacity : Int }

input outgoingShip : Stream String

output totalCapacity : Varying Int
output totalCapacity =
    Varying.map (List.sum << Dict.values) dock
```

### Running Effects in Elm

So far the only way to run effects like &ldquo;print this to the console&rdquo;
or &ldquo;talk to local storage&rdquo; has been through ports:

  1. Send a message through an `output`
  2. Run some effects in JavaScript
  3. Send the result back into Elm through an `input`.

This release introduces the concept of a `loopback` which brings this process
into Elm. Say you want to make an HTTP request whenever a user clicks a certain
button.

```elm
import Http
import JavaScript.Decode as JS

loopback stockQuotes : Stream (Result Http.Error Float)
loopback stockQuotes <-
    Stream.map getQuote stockSymbols

getQuote : String -> Promise Http.Error Float
getQuote symbol =
    Http.get JS.float ("http://finance.yahoo.com/lookup/" ++ symbol)
```

In this code, the `getQuote` function creates a *promise* to go talk to
Yahoo's finance API. We are not actually *doing* anything, just describing
how it should be done at some later time.

We send it through a `loopback` declaration to actually run the promise. At
that point, things start looking a lot like ports:

  1. A stream of promises go out (just like an `output`)
  2. The promises are run sequentially by Elm&rsquo;s runtime system
  3. The results come back into Elm (just like an `input`)


### Signal becomes Stream/Varying

The `Signal` module has been split into two separate concepts:

  * `Stream` is a stream of discrete events. It has no initial value or
    current value. It is nice for events like clicks or key presses.
  * `Varying` is a value that varies. It is always defined, changing at
    discrete moments. This is nice for things like `Mouse.position` or the
    current state of your application.

There is nothing fundamentally new here, the underlying implementation is
pretty much unchanged. The big difference is that the `Signal` API has been
split accross the `Stream` and `Varying` modules, hopefully clarifying things.

### Channel becomes Mailbox

As [architecture guidelines][arch] have developed, the concept of a `Channel`
got a bit messy. It grew to include `LocalChannel` and had some [fundamental
issues](https://github.com/elm-lang/elm-compiler/issues/889). This motivates
the new `Mailbox` abstraction.

[arch]: https://github.com/evancz/elm-architecture-tutorial

```elm
type alias Mailbox a =
    { address : Address a
    , stream : Stream a
    }

send : Address a -> a -> Promise x ()

forward : (a -> b) -> Address b -> Address a
```

A `Mailbox` has an `address` you can send values to, and a `stream` that
receives all of those values. The `forward` function lets you create
forwarding addresses that just send the message along, adding some extra
information. This is very helpful for writing more modular code.

The major change here is in how you *create* a mailbox. Rather than creating
them with a function, you use a top-level `loopback` declaration. This lets us
instantiate mailboxes in a safe way:

```elm
type Action = Increment | Decrement

loopback actions : Mailbox Action

incr : Promise x ()
incr =
  send actions.address Increment
```


"""
