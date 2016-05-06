import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


(=>) = (,)


main =
  Blog.blog
    "Everything as a Service"
    "How websockets made Elm 0.17 simpler"
    Blog.evan
    (Blog.Date 2016 5 9)
    [ Center.markdown "600px" intro1
    , img [ src "/assets/blog/everything-as-a-service.png", Center.style "561px" ] []
    , Center.markdown "600px" intro2
    , iframe
        [ style
            [ "display" => "block"
            , "width" => "306px"
            , "height" => "306px"
            , "margin" => "0 auto"
            , "border" => "none"
            ]
        , src "/examples/time/result"
        ]
        []
    , Center.markdown "600px" middle
    , iframe
        [ style
            [ "display" => "block"
            , "width" => "400px"
            , "height" => "200px"
            , "padding" => "20px"
            , "margin" => "0 auto"
            , "border" => "none"
            , "background-color" => "#eee"
            ]
        , src "/examples/websockets/result"
        ]
        []
    , Center.markdown "600px" rest
    ]


intro1 = """

<span style="color: red; font-weight: bold;">DRAFT - NOT FOR SHARING</span>

Elm is designed for ease-of-use. This means being just as easy to use as React
for creating web apps *and* providing guarantees like "no runtime errors" in
practice. Instead of producing stack traces whenever someone happens to stumble
upon a problem, Elm finds problems at compile time and [gives helpful
hints](/blog/compilers-as-assistants) immediately. **So what happens when Elm
needs to do something unreliable?** Calling JavaScript may crash at any time.
A websockets might go down for tons of reasons. How can we do the unreliable
stuff and keep Elm's core guarantees?

**The trick is to treat unreliable code as a service.** Create a client/server
relationship and just send messages between Elm and the crazy stuff. Elm 0.17
(1) introduces the concepts of *subscriptions* to make this client/server
communication easy and (2) uses this pattern for geolocation, websockets, and
JavaScript interop.

"""

intro2 = """

**Subscriptions** are the core new idea in Elm 0.17, so that is what we will
focus on in the rest of this post. There are other new things though! Elm 0.17
also includes:

  - Faster HTML renderer (numbers coming soon!)
  - Easier alternative for `Signal.Address`
  - Libraries for geolocation, page visibility, and web sockets
  - Generated JS is smaller and works with Google's Closure Compiler
  - Generated JS works with RequireJS and CommonJS
  - Features in place for services like GraphQL and Elixir Phoenix
  - Improved documentation at [guide.elm-lang.org][guide]
  - Helpful messages when decoding JSON fails

Lots of improvements! In the end, I think Elm does more than ever, and in
a weird twist of fate, that also made it easier to learn than ever!

> **Note:** Existing users should read the [upgrade plan][plan]. Then read the
new [guide][], especially the sections on [The Elm Architecture][arch] and
[ports][].

[plan]:  https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md
[guide]: http://guide.elm-lang.org
[arch]:  http://guide.elm-lang.org/architecture/index.html
[ports]: http://guide.elm-lang.org/interop/javascript.html


## What are subscriptions?

A subscription is a way for a component to sit around and wait for messages.
You can subscribe to the current time. You can subscribe to location changes.
You can subscribe to web socket messages. Etc.

The simplest subscription is time. It is the basis of this SVG clock:

"""



middle = """

Quickly check out [the source code](/examples/time) for this program. It is
very typical Elm code, following The Elm Architecture. The only new part is
the following definition:

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick
```

Based on the current model, we are describing all the active subscriptions. In
this case, the [`Time.every`][every] function is setting up a subscription to
the current time, updated every second. New times (like `Tick 1462487781991`)
are fed into the `update` function, just like messages resulting from mouse
clicks or HTTP requests.

[every]: http://package.elm-lang.org/packages/elm-lang/core/latest/Time#every

So besides subscriptions, everything about The Elm Architecture is the same as
before. It is just easier to passively wait for stuff. Let's see this in a
more difficult scenario.


## Web Sockets

Here is a simple chat client. To avoid people saying crazy things to each
other, we have it pointed at a chat server that just echos your messages:

"""


rest = """

Check out [the source code](/examples/websockets) real quick. It looks *very*
similar to the clock example, but it is actually managing a web socket
connection! All you have to think about are the following functions:

```elm
  WebSocket.send "ws://echo.websocket.org" input
  WebSocket.listen "ws://echo.websocket.org" NewMessage
```

The first one sends messages, the second one subscribes to messages. In both
cases we provide the address of the chat server we care about and a value that
helps us communicate with it. For `send` we just give it the string we want to
send. For `listen` we have a tagger function, so when we receive a message
like `"hi"` from the server, it is fed into our `update` function as
`NewMessage "hi"`.

Now the crazy thing is that **we wrote a fully functional chat client *without*
doing a bunch of shenanigans to set everything up.** We send and we listen.
That is it. I mean, it doesn't actually sound too crazy unless you know the two
options you have in JavaScript:

  - **Use the browser API** &mdash; To get started you just create a new web
  socket! Well, then you need to open the connection. But do not forget to add
  an `onerror` listener to detect when the connection goes down and try to
  reconnect with an exponential backoff strategy. And definitely do not `send`
  messages while the connection is down, that is a runtime error! You need to
  queue messages and send them later. So once you have your queue implemented,
  that's it! Now you just need to decide which component owns this web socket
  and make sure that the socket is closed at the appropriate time.

  - **Use a JS library** &mdash; Using the browser API seems like trouble, so
  let's just find a library that does all that. Looks like we have [more than
  2000 options](https://www.npmjs.com/search?q=websocket). Hmm... Hopefully
  one of the first few we try works well.

In the Web Sockets package for Elm, all of the babysitting of the connection
is handled automatically. The connection is opened if anyone is subscribed to
it, and it is closed if no one needs it anymore. All the queuing and
reconnecting happens behind the scenes.


## Learning More

That was a super brief introduction to subscriptions. If you want to really
understand what is going on with Elm and The Elm Architecture, check out
[guide.elm-lang.org][guide]. The section on [The Elm Architecture][arch] slowly
builds up to subscriptions and has a bunch of nice examples.

Again, experienced Elm users should read the [upgrade plan][plan]. I know you
know Elm, but you should read [guide.elm-lang.org][guide] anyway, especially
the sections on [The Elm Architecture][arch] and [ports][].

[plan]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md
[guide]: http://guide.elm-lang.org/
[arch]: http://guide.elm-lang.org/architecture/index.html
[ports]: http://guide.elm-lang.org/interop/javascript.html


## What is Next?

This release began with [a vision of how Elm will handle "native" code][forum].
What would a healthy package ecosystem look like? Elm 0.17 is the foundation
for this vision. We are now set up to support (1) everything in web platform
and (2) all sorts of custom backends like GraphQL, Elixir Phoenix, Firebase,
etc. All of these cases can be handled in terms of *commands* and
*subscriptions*, creating a consistent experience across packages that works
great with The Elm Architecture. I am excited to see this start rolling out in
the coming weeks and months!

[forum]: https://groups.google.com/forum/#!topic/elm-dev/1JW6wknkDIo

But how to get there? Well, [the web platform](https://platform.html5.org/) is
not really that much stuff when you look at it seriously, so the
[@elm-lang](https://github.com/elm-lang) organization will expand to cover the
remaining APIs. This is best because:

  1. I do not expect to be compiling to JavaScript forever. The smaller the
  interface between Elm and JS, the easier it will be to support other
  platforms.

  2. We do not want four okay versions of bindings to web platform APIs. One
  great version is better.

I know some people are eager to help with creating these libraries. Please give
me some time to develop a coherent process for making sure a desire to help can
also translate into great results. In the meantime, the best way to make
progress is to meet people on [the Elm slack](http://elmlang.herokuapp.com/)
and learn what is going on in the community. (You should always do that before
you start trying to make contributions!)


## Thank You

First I want to thank [@JustusAdam](https://github.com/JustusAdam) who did a
revamp of the `elm-reactor` navigation pages. This contribution was made ages
ago, but it is finally out and it looks great! I also want to thank all the
folks who worked through the overall design of 0.17 with me. I know it got
crazy after a certain amount of iterations, so thanks for following along and
giving great input! Finally, I want to thank the Elm community for (1) their
patience waiting for 0.17 and (2) testing the pre-release binaries out and
finding bugs. Special thanks to [@gdotdesign](https://github.com/gdotdesign),
[@colinmccabe](https://github.com/colinmccabe), and
[@lukewestby](https://github.com/lukewestby) who all found sneaky issues in
the new HTML renderer and created great [SSCCE](http://sscce.org/)'s.

"""
