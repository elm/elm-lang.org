import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


(=>) = (,)


main =
  Blog.blog
    "A Farewell to FRP"
    "Making signals unnecessary with The Elm Architecture"
    Blog.evan
    (Blog.Date 2016 5 10)
    [ Center.markdown "600px" start
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
            , "width" => "300px"
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


start = """

[The Elm Architecture][arch] is a simple pattern for architecting web apps. It
is the standard way to write Elm code, and with derivatives like Redux, it is
becoming a popular way to write JavaScript code too! So it is having success,
but we still hear questions like: How can I use websockets in The Elm Architecture?
Or GraphQL? Or geolocation? Well, **Elm 0.17 is out today, and it introduces
*subscriptions*** which cover these cases in a really pleasant way.
Subscriptions let components sit around and wait for messages while library
code handles a bunch of tricky resource management stuff behind the scenes.
Later in this post we will see how this makes websockets super simple to work
with.

That is all nice, but the big benefit is that **Elm is now significantly easier
to learn and use.** As the design of subscriptions emerged, we saw that all the
toughest concepts in Elm (signals, addresses, and ports) could collapse into
simpler concepts in this new world. Elm is *designed* for ease-of-use, so I was
delighted to stumble upon a path that would take us farther with fewer
concepts. To put this in more alarmist terms, **everything related to signals
has been replaced with something simpler and nicer.** There are two typical
reactions to this news:

  1. **This is crazy. How will anything work?!** I'd estimate that 95% of code
  stays exactly the same, and the [upgrade plan][plan] will walk you through
  the couple things you need to update. It is not actually a big deal.

  2. **What is this guy talking about? What is FRP? What are signals?** The
  cool thing about this release is that you do not need to know about that
  stuff anymore. Elm is just easier now.

In both cases, the best way to proceed is to just show how things work in the
new version of Elm. In the end, Elm does more than ever, but is also simpler
than ever. I am really happy with how it turned out, and I hope you enjoy it
too!

> **Note:** Other cool stuff in Elm 0.17 includes:
>
>  - Faster HTML renderer (numbers coming soon!)
>  - Libraries for geolocation, page visibility, and web sockets
>  - Generated JS is smaller and works with Google's Closure Compiler
>  - Generated JS works with RequireJS and CommonJS
>  - Features in place for services like GraphQL and Elixir Phoenix
>  - Improved documentation at [guide.elm-lang.org][guide]
>  - Helpful messages when decoding JSON fails


[plan]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md
[arch]: http://guide.elm-lang.org/architecture/index.html
[guide]: http://guide.elm-lang.org/


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

[every]: http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Time#every

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

In [the WebSockets package][ws] for Elm, all of the babysitting of the connection
is handled automatically. The connection is opened if anyone is subscribed to
it, and it is closed if no one needs it anymore. All the queuing and
reconnecting happens behind the scenes.

[ws]: http://package.elm-lang.org/packages/elm-lang/websocket/1.0.0/WebSocket


## Learning More

That was a super brief introduction to subscriptions. If you want to really
understand what is going on with Elm and The Elm Architecture, check out
[guide.elm-lang.org][guide]. The section on [The Elm Architecture][arch] slowly
builds up to subscriptions and has a bunch of nice examples.

Experienced Elm users should read the [upgrade plan][plan]. You should read
[guide.elm-lang.org][guide] too. I know you know Elm already, but I think the
guide really shows how all the parts of Elm 0.17 fit together in a nice way.
The sections on [The Elm Architecture][arch] and [ports][] are particularly
important for you.

And remember, you can always come talk to us on [the Elm Slack channel][slack]!
We are a friendly bunch that is happy to help folks learning new stuff or
upgrading old code. Just ask!

[plan]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md
[guide]: http://guide.elm-lang.org/
[arch]: http://guide.elm-lang.org/architecture/index.html
[ports]: http://guide.elm-lang.org/interop/javascript.html
[slack]: http://elmlang.herokuapp.com/


## A Farewell to FRP

Elm is about making delightful projects. Stuff like this [raycaster][].
Projects you are excited to share. Projects that get you excited about
programming! That means I am always asking myself how Elm can be simpler. How
can it be easier to learn? More fun? Quicker for prototyping? More reliable?
I think my obsession with these questions are the heart of Elm's design
philosophy and Elm's success.

[raycaster]: https://twitter.com/krisajenkins/status/726043742180925440

When I started working on [my thesis][thesis] in 2011, I stumbled upon this
academic subfield called Functional Reactive Programming (FRP). By stripping
that approach down to its simplest form, I ended up with something way easier
to learn than similar functional languages. Signals meant piles of difficult
concepts just were not necessary in Elm.

[thesis]: /assets/papers/concurrent-frp.pdf

I think anyone who has taught Elm recently would agree that signals are one of
the few stumbling blocks left. They made Elm easier than its peers, but they
did not make Elm *easy*.

As The Elm Architecture emerged, it became clear that you could do almost all
your Elm programming without thinking about signals at all. So the [start-app][]
package was an experiment to see what happens when we push signals way later in
the learning path. The results were great! Folks were getting started quicker,
making it farther, and having more fun! In the end, we had lots of folks who
became excellent Elm programmers without ever really learning much about
signals. They were not necessary. Elm 0.17 is just taking the next logical step.

[start-app]: https://github.com/evancz/start-app

In the end, it was possible to remove signals because Elm has been moving
towards an explicit emphasis on concurrency for quite some time now. The seeds
for this are obvious in my thesis, but the wheels really started turning on
this in Elm 0.15 when tasks were introduced. That release also introduced a
scheduler that was able to switch between work whenever it wanted. Elm 0.17
improves this scheduler quite significantly, taking some basic insights from
the BEAM VM used by Erlang and Elixir. You can read a tiny bit about
[how it works][docs1] and [where it is going][docs2] in the `Process` module
docs. This is also the foundation for [effect managers][mgmt], which make
subscriptions possible in the first place. I hope to flesh out the
documentation on this much more, but the nice thing is that you do not need to
know this stuff to be an Elm expert. Just like with my thesis,
[Concurrent FRP][thesis], the goal is to get the benefits of concurrency for
free.

[docs1]: http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Process
[docs2]: http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Process#future-plans
[mgmt]: http://guide.elm-lang.org/effect_managers/index.html

So is Elm about FRP anymore? No. Those days are over now. Elm is just a
functional language that takes concurrency very seriously. And from a user's
perspective, Elm is just a friendly functional language!

> **Note:** Interested readers may find [Lucid Synchrone][lucid] interesting.
Unfortunately for me, I had no idea my thesis had so much in common with
synchronous programming languages at the time, but the connections are quite
striking. I might argue that Elm was *never* about FRP.

[lucid]: http://www.di.ens.fr/~pouzet/bib/chap_lucid_synchrone_english_iste08.pdf


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

  1. I do not expect to be compiling to JavaScript forever, especially with
  WebAssembly on the horizon. The smaller the interface between Elm and JS,
  the easier it will be to support other platforms.

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
