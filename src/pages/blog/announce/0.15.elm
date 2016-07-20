import Blog
import Center


main =
  Blog.blog
    "Elm 0.15"
    "Asynchrony with Tasks"
    Blog.evan
    (Blog.Date 2015 4 20)
    [ Center.markdown "600px" content ]


content = """

This release introduces **tasks**, a way to define complex asynchronous
operations. Similar to [C#&rsquo;s tasks][csharp] and [JavaScript&rsquo;s
promises][promise], tasks make it simple to describe long-running effects and
keep things responsive. They also provide a way to wrap up tons of browser
APIs in Elm.

[csharp]: https://msdn.microsoft.com/en-us/library/hh191443.aspx
[promise]: http://www.html5rocks.com/en/tutorials/es6/promises/

Thanks to tasks, we have a couple new packages that make practical development
easier:

  * [elm-http][] &mdash; get JSON and strings from servers with a nice high-level API.
  * [elm-history][] &mdash; easily navigate browser history from Elm
  * [elm-router][] &mdash; generate pages dynamically based on the URL
  * [elm-storage][] &mdash; work with local storage

[elm-http]: http://package.elm-lang.org/packages/evancz/elm-http/1.0.0/
[elm-history]: https://github.com/TheSeamau5/elm-history/
[elm-router]: https://github.com/TheSeamau5/elm-router/
[elm-storage]: https://github.com/TheSeamau5/elm-storage/

Knowing the monstrosity that is XMLHttpRequest, it is really great to see that
functionality exposed in [elm-http][] without the atrocious parts. And this is
just the start! In the next weeks and months, the Elm community is going to be
filling in a lot of gaps by creating libraries for APIs like local storage,
geolocation, dropbox.js, firebase, etc. etc.

This release also marks a milestone for Elm in the sense that the fundamentals
are pretty much worked out. As soon as this release goes out, we will be
focusing on improving documentation and making our testing tools great. We
expect we will have one or two more releases before 1.0 to polish syntax and
core libraries based on real-world usage of tasks.


## Motivation

Since the release of [elm-html][], we have seen more and more people writing
practical web apps in Elm. [Richard Feldman](https://twitter.com/rtfeldman)
recently rewrote his writing app [Dreamwriter](https://dreamwriter.co/) from
[React and CoffeeScript](https://github.com/rtfeldman/dreamwriter-coffee/tree/strangeloop)
to [Elm and CoffeeScript](https://github.com/rtfeldman/dreamwriter/tree/strangeloop),
which has been a very interesting case study.

[elm-html]: /blog/blazing-fast-html

Richard took the approach of rewriting the core of Dreamwriter in Elm, and then
slowly expanding that core to cover as much as possible. This means he was able
to switch over gradually, and leave things in CoffeeScript if they were
working fine. We have noticed a couple really nice benefits so far:

  * **The Elm code just does not cause runtime errors in practice.**
    The bugs and crashes are always coming from the CoffeeScript code.

  * **Refactoring is super easy in the Elm section.** In JS or CoffeeScript,
    changing a function name or changing a data representation usually causes
    a cascade of changes that are quite hard to track down, even when you have
    great test coverage. In Elm, you can be confident that the compiler will
    tell you *all* the places that need to be updated as a result of your
    changes. Richard can change stuff in Elm and be shockingly confident that
    it will not quietly break some seemingly unrelated feature.

  * **Rendering is extremely fast.** [elm-html][] makes it really simple to
    optimize by just sprinkling [`lazy`][lazy] into your rendering code.

[lazy]: http://package.elm-lang.org/packages/evancz/elm-html/3.0.0/Html-Lazy

At this point, Richard's big question is &ldquo;how can we write more in
Elm?&rdquo; So this release is all about how we can write more in Elm and
*keep* all the great benefits that make it worthwhile to use Elm in the first
place.


## Introducing Tasks

The biggest part of this release is introducing **tasks**. Tasks make it easy
to describe asynchronous operations that may fail, like HTTP requests or
writing to a database. Tasks also work like light-weight threads in Elm, so
you can have a bunch running at the same time and the [runtime][rts] will hop
between them if they are blocked. As a simple example, let’s get the README
for Elm&rsquo;s core libraries from the
[Elm Package Catalog](http://package.elm-lang.org/).

[rts]: http://en.wikipedia.org/wiki/Runtime_system

```elm
import Http

pkgUrl =
  "http://package.elm-lang.org/packages/elm-lang/core/2.0.0/README.md"

getReadme : Task Http.Error String
getReadme =
  Http.getString pkgUrl
```

So `getReadme` is a `Task` that can be performed by Elm&rsquo;s runtime. When
we run the task, it will either fail with an [`Http.Error`][error] or succeed
with a string of markdown.

[error]: http://package.elm-lang.org/packages/evancz/elm-http/1.0.0/Http#Error

To actually perform a task, you send it out a [port][]. Currently Richard sends
certain values out to CoffeeScript which performs all sorts of effects and then
lets Elm know about it once they are done. That means some logic ends up in
CoffeeScript. Tasks let you describe all that logic in Elm, so Richard can
describe the whole task in Elm and send it to Elm&rsquo;s runtime which will
go through and make it all happen. The end result is the same, but now Richard
has a code base that is easier to refactor and debug!

[port]: http://guide.elm-lang.org/interop/javascript.html

To learn more about tasks, check out [the tutorial](/guide/reactivity#tasks) and then
the [zip codes](/examples/zip-codes) and
[flickr](/examples/flickr) examples!


## Faster Text Rendering

One of our commercial users, [CircuitHub](https://www.circuithub.com/), has
been using collages to render complex circuits. The performance bottleneck
for them was text rendering, so thanks to
[James Smith](https://github.com/jazmit), we added a simple function that let
us render to canvas much more efficiently:

```elm
Graphics.Collage.text : Text -> Form
```

We get to reuse the whole [`Text`](http://package.elm-lang.org/packages/elm-lang/core/2.0.0/Text)
API but we then render direct to canvas to get much better performance. I am
looking forward to seeing this used in practice!

As part of this change, we moved a few functions out of the `Text` library to
clean up the API. Here is a rough listing of the functions that have moved
into the `Graphics.Element` library:

```elm
leftAligned : Text -> Element
centered : Text -> Element
rightAligned : Text -> Element

show : a -> Element   -- was Text.asText
```

The goal here is to make `Text` an abstract representation that can be rendered
in many different contexts. The `Text` module should not know what an `Element`
or a collage is.

Keep an eye out for this when you are upgrading! You will need to mess with any
uses of `leftAligned` to get everything working. In the process of upgrading
this website to Elm 0.15 I found this often reduced the number of imports I
needed by quite a lot, especially in smaller beginner examples that used
`asText`.


## Towards &ldquo;No Runtime Exceptions&rdquo;

We are currently at a point where you *practically* never get runtime
exceptions in Elm. I mean, you can do it, but you have to try really hard.

That said, there are a few historical relics in the `List` library that *can*
cause a crash if they are given an empty list. Stuff like `head` and `tail` are
pretty easy to run into if you are a beginner. This is primarily because older
languages in the tradition of Elm made this choice and it felt weird to diverge,
especially when Elm was younger. This release changes these functions to give
back a `Maybe` and sets us up for avoiding unintended runtime exceptions
*entirely*.

So the new `List` library looks like this:

```elm
head : List a -> Maybe a
tail : List a -> Maybe (List a)

maximum : List comparable -> Maybe comparable
minimum : List comparable -> Maybe comparable
```

This will mean beginners will be a lot less likely to get runtime errors, and
hopefully help people form better habits.

Aside: We are considering adding two functions to the `Maybe` library
([this][default] and [this][unsafe]) as part of the move towards &ldquo;no
runtime errors&rdquo; so please share your relevant experiences and examples
as you work with 0.15!

[default]: https://github.com/elm-lang/core/issues/216
[unsafe]: https://github.com/elm-lang/core/issues/215


## Import Syntax

We dramatically reduced the set of default imports in 0.14. This was &ldquo;the
right thing to do&rdquo; but it made our existing import syntax feel a bit
clunky. You needed a pretty big chunk of imports to get even basic programs
running. This release introduces improved syntax that will let you cut a
bunch of lines from your import section. As a brief preview, let&rsquo;s look
at the two extremes of the syntax. First we have a plain old import:

```elm
import Http
```

With this, we can refer to any value in the `Http` module as `Http.get`
or `Http.post`. Using qualified names like this is recommended, so the simple
import syntax should cover most typical cases. Sometimes you want to go crazy
though, so on the other end of the spectrum, we have a way to choose a shorter
prefix with `as` and a way to directly expose some values with `exposing`.

```elm
import Html.Attributes as Attr exposing (..)
```

In this case we decided to expose *everything* in `Html.Attributes` so we can
just say things like [`id`][id] and [`href`][href] directly. We also
locally rename the module to `Attr` so if there is ever a name collision, we
can say [`Attr.width`][width] to make it unambiguous. You can read more about
the new import syntax [here](/docs/syntax#modules).

[id]: http://package.elm-lang.org/packages/evancz/elm-html/3.0.0/Html-Attributes#id
[href]: http://package.elm-lang.org/packages/evancz/elm-html/3.0.0/Html-Attributes#href
[width]: http://package.elm-lang.org/packages/evancz/elm-html/3.0.0/Html-Attributes#width

This syntax seems like a minor change, but it has made a huge difference in how
it feels to work with imports. I have been really happy with it so far. When
you are upgrading your code, keep an eye out for:

  * Needing to add `exposing` keyword.
  * Importing the same module on two lines. This can now be reduced to one line.
  * Importing [default modules](https://github.com/elm-lang/core#default-imports).
    They come in by default, so there is no need to explicitly import `Signal`
    or `List` unless you are doing something special. (We are planning to add
    warnings for this in a future release to make this easier!)


## Introducing Mailboxes

[The Elm Architecture][arch] is all about creating nestable and reusable
components. In 0.14 this meant using channels and the [local-channel][]
package. The terminology and API were kind of messy because parts of it evolved
*after* 0.14 came out, making things seem artificially complex. So with 0.15
we are revamping this whole API so that it is centralized and easier to learn.

[arch]: https://github.com/evancz/elm-architecture-tutorial/#the-elm-architecture
[local-channel]: https://github.com/evancz/local-channel/

The new `Signal` library introduces the concept of a `Mailbox`.

```elm
type alias Mailbox a =
    { address : Address a
    , signal : Signal a
    }
```

A mailbox has two key parts: (1) an address that you can send messages to and
(2) a signal that updates whenever a message is received. This means you can
have `onClick` handlers in your HTML report to a particular address, thus
feeding values back into your program as a signal.

There are a few ways to talk to a particular mailbox. The most common is via
the event handlers in [`Html.Events`][events]. For example, the `onClick`
function takes an address to send to and a value to send.

[events]: http://package.elm-lang.org/packages/evancz/elm-html/3.0.0/Html-Events

```elm
onClick : Address a -> a -> Attribute
```

So if we say `(onClick addr 42)` Elm will send the value `42` to the mailbox
with address `addr` whenever the user clicks the corresponding HTML element.
This lets us feed user input from the UI into our program. This pattern is
described in more detail in [the architecture tutorial][arch].

The second most common way comes in handy when you are working with packages
like [elm-http][] that use tasks.

```elm
send : Address a -> a -> Task x ()
```

You provide an address and a value to send, creating a task. When that task is
performed, that value shows up at the corresponding mailbox. It&rsquo;s kinda
like real mailboxes!

Check out [the task tutorial](/guide/reactivity#tasks) for more examples and
explanation of mailboxes. For those of you with 0.14 code to upgrade, first
take a look at [the whole API][mailbox] to get a feel for it. The core concepts
are pretty much the same, so the changes are mostly find and replace:

[mailbox]: http://package.elm-lang.org/packages/elm-lang/core/2.0.0/Signal#Mailbox

  * `Signal.Channel` becomes `Signal.Mailbox` in your types
  * `Signal.channel` becomes `Signal.mailbox` when creating mailboxes
  * `Signal.send` becomes `Signal.message` in your event handlers
  * `(Signal.subscribe channel)` becomes `mailbox.signal`
  * Any talk of `LocalChannel` is replaced by `Address` and [`forwardTo`][forwardTo]
  * Handlers like `onClick` have a simpler API with the latest [elm-html][]

[forwardTo]: http://package.elm-lang.org/packages/elm-lang/core/2.0.0/Signal#forwardTo


## Return of the `elm` command

With the introduction of the parallel build tool `elm-make` the stand-alone
`elm` command disappeared for a bit. Well now it is back! Similar to the `git`
command it serves as a way to get to all the relevant command line tools, and
lets you extend these tools in a seamless way. When you run the `elm` command
alone, it will tell you all the possibilities available to you:

```
Elm Platform 0.15 - a way to run all Elm tools

Usage: elm <command> [<args>]

Available commands include:

  make      Compile an Elm file or project into JS or HTML
  package   Manage packages from <http://package.elm-lang.org>
  reactor   Develop with compile-on-refresh and time-travel debugging
  repl      A REPL for running individual expressions

You can learn more about a specific command by running things like:

  elm make --help
  elm package --help
  elm <command> --help

In all these cases we are simply running 'elm-<command>' so if you create an
executable named 'elm-foobar' you will be able to run it as 'elm foobar' as
long as it appears on your PATH.
```

I hope this message will make it easier for people to get started with Elm.
Even after programming for quite a few years, I sometimes feel very helpless
after installing a new command line tool or learning about some existing
command. I hope the `elm` command will make it really clear what can be done
and give some hints about how to learn more.


## Thank you

More so than normal, this release went through a pretty crazy design and
exploration phase, so I want to give a huge thank you to everyone who was part
of that process. I think we put together a ton of great ideas that will make
their way into Elm soon enough!

[list]: https://groups.google.com/forum/#!forum/elm-discuss

Thank you to [Elm User Group SF](http://www.meetup.com/Elm-user-group-SF/)
which worked with some pre-release versions of 0.15 to vet the tasks API and
start making some new packages for browser APIs.

"""






