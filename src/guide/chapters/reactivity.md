# Reactivity

In the last section we learned about [the Elm Architecture][arch] which provides a reliable foundation for your application. In this section, we will see how that fits into a system that talks to servers, uses websockets, writes to databases, etc. The following diagram gives a high-level overview:

[arch]: /guide/architecture

<img src="/assets/diagrams/overall-architecture.png" style="width: 100%;"/>

The main idea is that the vast majority of our application logic can be described in terms of [the Elm Architecture][arch], allowing us to make the most of stateless functions and immutability. In this realm we use [signals](#signals) to route events to the right place.

When we want to have some effect on the world, we create a service totally separate from our core logic. Each service may be in charge of a specific task, like managing database connections or communicating via websockets. We script all of these effects with [tasks](#tasks).

We get a lot of benefits by cleanly separating out these services. Most obviously, testing is much simpler. We can make sure our UI and application logic work without doing crazy tricks to mock a database.


## Signals

Signals route events in the core logic of your application, which should use
[the Elm Architecture][arch]. So far we have hidden these details with the
[start-app][] package, which is just a very simple wrapper around signals.

You can think of signals as setting up a static [processing network][kpn],
where a fixed set of inputs receive messages that propagate through the
network, ultimately leading to outputs that handle stuff like efficiently
rendering things on screen.

[start-app]: https://github.com/evancz/start-app
[kpn]: http://en.wikipedia.org/wiki/Kahn_process_networks

<img src="/assets/diagrams/signals.png" style="width: 100%;"/>

This is actually the shape of the processing network in most Elm programs.
All of the state of our application lives in the `foldp`, and we mainly use
signals on the borders to route incoming and outgoing events.

> **Note:** You are probably thinking, &ldquo;All the state in one place?! What about encapsulation?!?!&rdquo; Before you close the tab, think about this like a database person: the hardest problems when managing state is **consistency**. How do I ensure that making a change in one component is properly propagated everywhere else? How do I know this component is looking at the latest state? As you have more and more components in your system, these questions become more and more complex. In your personal experience with JS, state inconsistencies are probably the primary source of bugs.

**In Elm we separate consistency from modularity.** The big `foldp` is a
centralized data store that ensures consistency, and [the Elm Architecture][arch]
is the pattern that keeps our programs modular. By separating these concerns,
we can do better at both.


We build up these networks using a relatively focused API in the
[`Signal`][signal] module:

[signal]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal

```elm
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
tasks running at the same time and the [runtime][rts] will hop between them if
they are blocked.

[rts]: http://en.wikipedia.org/wiki/Runtime_system

This tutorial is going to slowly build up to some realistic examples of HTTP
requests with the [elm-http][] package, like looking up [zip codes][zip] and
querying [flickr][]. This API is a ton nicer than XMLHttpRequest and has some
benefits over JavaScript&rsquo;s promises when it comes to error handling. But
like I said, we will build up to this slowly so stick with this tutorial until
then!

[zip]: /examples/zip-codes
[flickr]: /examples/flickr

To get started, install the `evancz/task-tutorial` package in your working
directory by running the following command:

```bash
elm-package install evancz/task-tutorial -y
elm-package install evancz/elm-html -y
elm-package install evancz/elm-http -y
elm-package install evancz/elm-markdown -y
```

This exposes the `TaskTutorial` module which has [a few values][task-tutorial]
that will help build a foundation for working with tasks.

[task-tutorial]: http://package.elm-lang.org/packages/evancz/task-tutorial/latest/TaskTutorial

### Basic Example

Let’s start out with a very simple function for printing values out to the
console:

```elm
print : a -> Task x ()
```

We give the [`print`][print] function a value, and it gives back a `Task` that
can be performed at some point and will print that value out. The `x` is a
placeholder that normally says what kind of errors can happen, but try not to
get hung up on it too much at this point. We will come back to it! The
important thing is that we have a task for printing stuff out.

[print]: http://package.elm-lang.org/packages/evancz/task-tutorial/latest/TaskTutorial#print

To actually *perform* a task, we hand it to a [port][]. Think of ports as a
way of asking the Elm runtime to do something for you. In this case, it means
*run the task*. So let’s see an example that puts `print` together with ports
to print out the current time every second.

[port]: /guide/interop

```elm
import Graphics.Element exposing (show)
import Task exposing (Task)
import TaskTutorial exposing (print)
import Time exposing (second, Time)


-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every second


-- Turn the clock into a signal of tasks
printTasks : Signal (Task x ())
printTasks =
  Signal.map print clock


-- Actually perform all those tasks
port runner : Signal (Task x ())
port runner =
  printTasks


main =
  show "Open your browser's Developer Console."
```

When we initialize this module we will see the current time printed out every
second. The `printTasks` signal is creating a bunch of tasks, but that does not
do anything on its own. Just like in real life, creating a task does not mean
the task magically happens. I can write &ldquo;buy more milk&rdquo; on my todo
list as many times as I want, but I still need to go to the grocery store and
buy it if I want the milk to appear in my refrigerator.

So in Elm, tasks are not run until we hand them to the runtime through a port.
This is similar to sending a record or list out a port, but instead of handing
it to some JavaScript callback, the runtime just performs the task.

We can give a port either a task or a signal of tasks. When you give a signal,
all the tasks will be performed in order without overlapping.


### Chaining Tasks

In the example above we used [`print`][print] but what if we want to create a
more complex task? Something with many steps.

First let’s introduce [`getCurrentTime`][now] so we can do more than print!

[now]: http://package.elm-lang.org/packages/evancz/task-tutorial/latest/TaskTutorial#getCurrentTime


```elm
getCurrentTime : Task x Time
```

This is a task that just gives you the current time. You run it, it tells you
what time it is. Now what we want to do is run [`getCurrentTime`][now] and
then [`print`][print] it out. Let’s look at the finished product and then
work through all the new parts.

```elm
import Graphics.Element exposing (show)
import Task exposing (Task, andThen)
import TaskTutorial exposing (getCurrentTime, print)


port runner : Task x ()
port runner =
  getCurrentTime `andThen` print


main =
  show "Open the Developer Console of your browser."
```

First, notice the infrequently-used backtick syntax which let’s us treat normal
functions as infix operators. As another example, `(add 3 4)` is the same as
``(3 `add` 4)``. So saying ``(getCurrentTime `andThen` print)`` is the same as
saying `(andThen getCurrentTime print)`. The only thing is that it reads a bit
more like English when using the backtick syntax.

Okay, now that we know that [`andThen`][andThen] is a normal function that
takes two arguments, let’s see the type.

[andThen]: http://package.elm-lang.org/packages/elm-lang/core/latest/Task#andThen

```elm
andThen : Task x a -> (a -> Task x b) -> Task x b
```

The first argument is a task that we want to happen, in our example this is
`getCurrentTime`. The second argument is a callback that creates a brand new
task. In our case this means taking the current time and printing it.

It may be helpful to see the slightly more verbose version of our task chain:

```elm
printTime : Task x ()
printTime =
  getCurrentTime `andThen` print


printTimeVerbose : Task x ()
printTimeVerbose =
  getCurrentTime `andThen` \\time -> print time
```

These are both exactly the same, but in the second one, it is a bit more
explicit that we are waiting for a `time` and then printing it out.

The [`andThen`][andThen] function is extremely important when using tasks
because it lets us build complex chains. We will be seeing more of it in
future examples!


### Communicating with Mailboxes

So far we have just been performing tasks and throwing away the result. But
what if we are getting some information from a server and need to bring that
back into our program? We can use a [`Mailbox`][mb], just like when
[constructing UIs][arch] that need to talk back! Here is the definition from
the [`Signal`][signal] module:

[mb]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal#Mailbox
[arch]: https://github.com/evancz/elm-architecture-tutorial/
[signal]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal

```elm
type alias Mailbox a =
    { address : Address a
    , signal : Signal a
    }

mailbox : a -> Mailbox a
```

A mailbox has two key parts: (1) an address that you can send messages to and
(2) a signal that updates whenever a message is received. You create a mailbox
by providing an initial value for the `Signal`.

For our purposes here, the [`send`][send] function is one major way to send
messages to a mailbox.

[send]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal#send

```elm
send : Address a -> a -> Task x ()
```

You provide an address and a value, and when the task is performed, that value
shows up at the corresponding mailbox. It&rsquo;s kinda like real mailboxes!
Let’s do a small example that uses `Mailbox` and `send`.

```elm
import Graphics.Element exposing (Element, show)
import Task exposing (Task, andThen)
import TaskTutorial exposing (getCurrentTime, print)


main : Signal Element
main =
  Signal.map show contentMailbox.signal


contentMailbox : Signal.Mailbox String
contentMailbox =
  Signal.mailbox ""


port updateContent : Task x ()
port updateContent =
  Signal.send contentMailbox.address "hello!"
```

This program starts out showing an empty string, the initial value in the
mailbox. We immediately start running the `updateContent` task which sends a
new message to `contentMailbox`. When it arrives, the value of
`contentMailbox.signal` updates and we start showing `"hello!"` on screen.

Now that we have a feel for `andThen` and for `Mailbox` let’s try a more
useful example!


### HTTP Tasks

One of the most common things you will want to do in a web app is talk to
servers. The [elm-http][] library provides everything you need for that, so
let&rsquo;s try to get a feel for how it works with the `Http.getString`
function.

```elm
Http.getString : String -> Task Http.Error String
```

We provide a URL, and it will create a task that tries to fetch the
resource that lives at that location as a `String`. Looking at the type of the
`Task`, finally that darn `x` is filled in with a real error type! This task
will either fail with some [`Http.Error`][error] or succeed with a `String`.

This exact function is actually used to load the README for packages in the
[Elm Package Catalog][epc]. Let’s look at the code for that!

[error]: http://package.elm-lang.org/packages/evancz/elm-http/latest/Http#Error
[epc]: http://package.elm-lang.org/

```elm
import Http
import Markdown
import Html exposing (Html)
import Task exposing (Task, andThen)


main : Signal Html
main =
  Signal.map Markdown.toHtml readme.signal


-- set up mailbox
--   the signal is piped directly to main
--   the address lets us update the signal
readme : Signal.Mailbox String
readme =
  Signal.mailbox ""


-- send some markdown to our readme mailbox
report : String -> Task x ()
report markdown =
  Signal.send readme.address markdown


-- get the readme *and then* send the result to our mailbox
port fetchReadme : Task Http.Error ()
port fetchReadme =
  Http.getString readmeUrl `andThen` report


-- the URL of the README.md that we desire
readmeUrl : String
readmeUrl =
  "https://raw.githubusercontent.com/elm-lang/core/master/README.md"
```

The most interesting part is happening in the `fetchReadme` port. We attempt to
get the resource at `readmeUrl`. If we succeed, we `report` it to the `readme`
mailbox. If we fail, the whole chain of tasks fails and no message is sent.

So assuming the server holding the README responds, we will see a blank screen turn
into the contents of the elm-lang/core readme!


### More Chaining

We have seen `andThen` used to chain two tasks together, but what if we want
to chain lots of tasks? This can end up looking a bit odd, so you can bend the
typical rules about indentation to make it look nicer. Let’s look at an example
that chains a bunch of tasks together to measure how long it takes to evaluate
the `(fibonacci 20)` expression:

```elm
import Graphics.Element exposing (show)
import Task exposing (Task, andThen, succeed)
import TaskTutorial exposing (getCurrentTime, print)
import Time exposing (Time)


getDuration : Task x Time
getDuration =
  getCurrentTime
    `andThen` \\start -> succeed (fibonacci 20)
    `andThen` \\fib -> getCurrentTime
    `andThen` \\end -> succeed (end - start)


fibonacci : Int -> Int
fibonacci n =
  if n <= 2 then
    1
  else
    fibonacci (n-1) + fibonacci (n-2)


port runner : Task x ()
port runner =
  getDuration `andThen` print


main =
  show "Open the Developer Console of your browser."
```

This reads fairly naturally. Get the current time, run the fibonacci function,
get the current time again, and then succeed with the difference between the
start and end time.

You might be wondering &ldquo;why is `start` in scope two tasks later?&rdquo;
The trick here is that an anonymous function includes everything after the
arrow. So if we were to put parentheses on our `getDuration` function, it
would look like this:

```elm
getDuration : Task x Time
getDuration =
  getCurrentTime
    `andThen` (\\start -> succeed (fibonacci 20)
    `andThen` (\\fib -> getCurrentTime
    `andThen` (\\end -> succeed (end - start))))
```

Now you can really see how weird our indentation is! The point is that you will
see this chaining pattern relatively often because it lets you keep a bunch of
variables in scope for many different tasks.


### Error Handling

So far we have only really considered tasks that succeed, but what happens when
an HTTP request comes back with a 404 or some JSON cannot be decoded? There are
two main ways to handle errors with tasks. The first is the
[`onError`][onError] function:

[onError]: http://package.elm-lang.org/packages/elm-lang/core/latest/Task#onError

```elm
onError : Task x a -> (x -> Task y a) -> Task y a
```

Notice that it looks very similar to `andThen` but it only gets activated when
there is an error. So if we want to recover from a bad JSON request, we could
write something like this:

```elm
import Graphics.Element exposing (show)
import Http
import Json.Decode as Json
import Task exposing (Task, andThen, onError, succeed)
import TaskTutorial exposing (print)


get : Task Http.Error (List String)
get =
  Http.get (Json.list Json.string) "http://example.com/hat-list.json"


safeGet : Task x (List String)
safeGet =
  get `onError` (\\err -> succeed [])


port runner : Task x ()
port runner =
  safeGet `andThen` print


main =
  show "Open the Developer Console of your browser."
```

With the `get` task, we can potentially fail with an `Http.Error` but when
we add recovery with `onError` we end up with the `safeGet` task which will
always succeed. When a task always succeeds, it is not possible to pin down
the error type. The type could be anything, we will never know because it will
never happen. That is why you see the free type variable `x` in the type of
`safeGet`.

The second approach to error handling is to use functions like
[`Task.toMaybe`][toMaybe] and [`Task.toResult`][toResult].

[toMaybe]: http://package.elm-lang.org/packages/elm-lang/core/latest/Task#toMaybe
[toResult]: http://package.elm-lang.org/packages/elm-lang/core/latest/Task#toResult

```elm
toMaybe : Task x a -> Task y (Maybe a)
toMaybe task =
  Task.map Just task `onError` \\_ -> succeed Nothing


toResult : Task x a -> Task y (Result x a)
toResult task =
  Task.map Ok task `onError` \\msg -> succeed (Err msg)
```

This is essentially promoting any errors to the success case. Let’s see it in
action.

```elm
import Graphics.Element exposing (show)
import Http
import Json.Decode as Json
import Task exposing (Task, andThen, toResult)
import TaskTutorial exposing (print)


get : Task Http.Error (List String)
get =
  Http.get (Json.list Json.string) "http://example.com/hat-list.json"


safeGet : Task x (Result Http.Error (List String))
safeGet =
  Task.toResult get


port runner : Task x ()
port runner =
  safeGet `andThen` print


main =
  show "Open the Developer Console of your browser."
```

With `safeGet` we can do our error handling with the `Result` type, which can
come in handy especially if you are working with certain APIs.


### Further Learning

Now that we have a foundation in chaining tasks with `andThen` and handling
errors, start taking a look at some of the examples out in the wild. Try to
adapt them to your case.

  * [zip codes][zip]
  * [flickr][]
