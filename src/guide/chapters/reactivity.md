# Reactivity

In the last section we learned about [the Elm Architecture][arch] which provides
a reliable foundation for your application. In this section will see how that
fits into a system that talks to servers, uses websockets, writes to databases,
etc. The following diagram gives a high-level overview:

[arch]: /guide/architecture

<img src="/assets/diagrams/overall-architecture.png" style="width: 100%;"/>

When working with the core logic, we use [signals](#signals) to manage and
route events to the right place. When working in a service, we use
[tasks](#tasks) to script effects like server interactions. Finally, we use
and [mailboxes](#mailboxes) to bridge the gap between these two worlds,
shuttling messages from services back to our core logic.


## Signals

Signals route events in the core logic of your application, which should use
[the Elm Architecture][arch]. So far we have hidden these details with the
[start-app][] package, which is just a very simple wrapper around signals.

You can think of signals as setting up a static [processing network][kpn],
where a fixed set of inputs receive messages that propegate through the
network, ultimately leading to outputs that handle stuff like efficiently
rendering things on screen.

[start-app]: https://github.com/evancz/start-app
[kpn]: http://en.wikipedia.org/wiki/Kahn_process_networks

<img src="/assets/diagrams/signals.png" style="width: 100%;"/>

This is actually the shape of the processing network in most Elm programs.
All of the state of our application lives in the `foldp`, and we mainly use
signals on the borders to route incoming and outgoing events.

> **Note:** You are probably thinking, &ldquo;All the state in one place?! What about encapsulation?!?!&rdquo; Before you close the tab, think about this like a database person: the hardest problems when managing state is **consistency**. How do I ensure that making a change in one component is properly propegated everywhere else? How do I know this component is looking at the latest state? As you have more and more components in your system, these questions become more and more complex. In your personal experience with JS, state inconsistencies are probably the primary source of bugs.

**In Elm we separate consistency from modularity.** The big `foldp` is a
centralized data store that ensures consistency, and [the Elm Architecture][arch]
is the pattern that keeps our programs modular. By separating these concerns,
we can do better at both.


We build up these networks using a relatively focused API in the
[`Signal`][signal] module:

[signal]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal

```haskell
map : (a -> b) -> Signal a -> Signal b

filter : (a -> Bool) -> a -> Signal a -> Signal a

merge : Signal a -> Signal a -> Signal a

foldp : (a -> s -> s) -> s -> Signal a -> Signal s
```

Definitely go through a few of [the signal examples](/examples) to get a feel
for this API. As we continue with this section, we will see how to hook this
basic routing mechanism into services that do HTTP requests and such.

> **Note:** It is usually best to use signals as little as possible. When it
comes to writing nice modular code, you should primarily use normal functions
and values. If you find yourself stuck with a signal of signals, ask yourself
&ldquo;how can I model this explicitly with functions and values?&rdquo;


## Tasks

Tasks make it easy to describe asynchronous operations that may fail, like
HTTP requests or writing to a database. Tons of browser APIs are described as
tasks in Elm:

  * [elm-http][] &mdash; talk to servers
  * [elm-history][] &mdash; navigate browser history
  * [elm-storage][] &mdash; save info in the users browser

[elm-http]: http://package.elm-lang.org/packages/evancz/elm-http/latest/
[elm-history]: https://github.com/TheSeamau5/elm-history/
[elm-storage]: https://github.com/TheSeamau5/elm-storage/

Tasks also work like light-weight threads in Elm, so you can have a bunch of
tasks running at the same time and the runtime will hop between them if they
are blocked.


## Mailboxes

Now that we have signals to route events and tasks to describe complex effects,
we need a way for them to talk. This is the role of mailboxes in Elm. You can
think of a mailbox as a way for services to talk back to the main Elm app.
