import Graphics.Element exposing (..)
import Markdown
import Website.Skeleton exposing (skeleton)
import Website.Tiles as Tile
import Window

port title : String
port title = "Elm 0.15"


main =
  Signal.map (skeleton "Blog" everything) Window.dimensions


everything wid =
  let w = min 600 wid
  in
    flow down
      [ width w content
      ]

content = Markdown.toElement """

<h1><div style="text-align:center">Elm 0.15
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">Asynchrony with Tasks</div></div>
</h1>

<span style="color:red;">DRAFT - NOT FOR DISTRIBUTION</span>

This release introduces **Tasks**, a way to define complex asynchronous
operations. Similar to [C#&rsquo;s tasks][csharp] and JavaScript&rsquo;s
promises, it makes it simple to describe long-running effects and keep things
responsive. It also provides a way to wrap up tons of browser APIs in Elm.

[csharp]: https://msdn.microsoft.com/en-us/library/hh191443.aspx

So as of today, we have a couple new packages that make practical development
easier:

  * [elm-http][] &mdash; get JSON and strings from servers with a nice high-level API.
  * [elm-history][] &mdash; easily navigate browser history from Elm
  * [elm-router][] &mdash; generate pages dynamically based on the URL

This is just the start though. In the next weeks and months, the community is
going to be filling in a lot of gaps by creating libraries for APIs like local
storage, geolocation, dropbox.js, firebase, etc. etc.

[elm-http]: http://package.elm-lang.org/packages/evancz/elm-http/latest/
[elm-history]: https://github.com/TheSeamau5/elm-history/
[elm-router]: https://github.com/TheSeamau5/elm-router/

This release also marks a milestone for Elm in the sense that the fundamentals
are pretty much worked out. As soon as this release goes out, I am going to
begin focusing on improving documentation and making our testing tools great.
I expect we will have one or two more releases before 1.0 to polish syntax and
core libraries based on real-world usage of tasks.


## Motivation

Since the release of [elm-html][], we have seen more and more people writing
practical web apps in Elm. [Richard Feldman](https://twitter.com/rtfeldman)
recently rewrote his writing app [Dreamwriter](https://dreamwriter.io/) from
React and CoffeeScript to Elm and CoffeeScript, which has been a very
interesting case study.

[elm-html]: /blog/Blazing-Fast-Html.elm

Richard took the approach of rewriting the core of Dreamwriter in Elm, and then
slowly expanding that core to cover as much as possible. This means he was able
to switch over gradually, and leave things in CoffeeScript if they were
working fine. We have noticed a couple really nice benefits so far:

  * The bugs and crashes are always coming from the CoffeeScript code. The Elm
    code just does not cause runtime errors in practice.

  * Refactoring is super easy in the Elm section. If some function or data
    format needs to change, this usually causes a cascade of changes that are
    super hard to track down in JS or CoffeeScript, even when you have great
    test coverage. In Elm, you can be confident that the compiler will tell you
    *all* the places that need to be updated as a result. You can change stuff
    without fear of accidentally breaking some seemingly unrelated feature.

  * Rendering is super fast. [elm-html][] makes it really simple to optimize
    by just sprinkling [`lazy`][lazy] into your rendering code.

[lazy]: http://package.elm-lang.org/packages/evancz/elm-html/latest/Html-Lazy

So Richard's question to me is &ldquo;how can we write more in Elm?&rdquo; Most
of his bugs and hard to refactor code is in CoffeeScript. For him, he will have
a better code base if he can move even more code into Elm. This release is
answering the question &ldquo;how can we write more in Elm and *keep* all the
great benefits that make it worthwhile to use Elm in the first place?&rdquo;


## Tasks

This release introduces **tasks** which let us describe asynchronous effects
and manages errors. As a simple example, let’s get the README for Elm&rsquo;s
core libraries.

```haskell
import Http

pkgUrl =
  "http://package.elm-lang.org/packages/elm-lang/core/latest/README.md"

getReadme : Task Http.Error String
getReadme =
  Http.getString pkgUrl
```

So `getReadme` is a `Task` that can be performed at some point in the future.
At that time the task will either fail with an [`Http.Error`][error] or
succeed with a string of markdown.

[error]: http://package.elm-lang.org/packages/evancz/elm-http/latest/Http#Error

To actually perform a task, you send it out a [port][]. Currently Richard sends
certain values out to CoffeeScript which performs all sorts of effects and then
lets Elm know about it once they are done. That means some logic ends up in
CoffeeScript. Tasks let you describe all that logic in Elm, so Richard can
describe the whole task in Elm and send it to Elm&rsquo;s [runtime system][rts]
which will go through and make it all happen. The end result is the same, but
now Richard has a code base that is easier to refactor and debug!

To learn more about tasks, check out [the tutorial](/learn/Tasks.elm)!


## Towards &ldquo;No Runtime Exceptions&rdquo;

We are currently at a point where you *practically* never get runtime
exceptions in Elm. I mean, you can do it, but you have to try really hard.

That said, there are a few historical relics in the `List` library that *can*
cause a crash if they are given an empty list. Stuff like `head` and `tail` are
pretty easy to run into if you are a beginner. This is primarily because older
languages in the tradition of Elm made this choice and it felt weird to diverge,
especially when Elm was younger. This release replaces these cases with
functions that give back a `Maybe` and sets us up for avoiding unintended
runtime exceptions *entirely*.

So the new `List` library looks like this:

```haskell
head : List a -> Maybe a
tail : List a -> Maybe a

maximum : List comparable -> Maybe comparable
minimum : List comparable -> Maybe comparable
```

We are thinking of adding two functions to `Maybe` in a later release to help
make it really pleasant to *always* return a `Maybe` when a function may fail.
The first one is just an alias for `withDefault` which would work like this:

```haskell
(?) : Maybe a -> a -> a

firstNumber =
    head numberList ? 0
```

If you want to get the head of a list of numbers *or* just go with zero if it
is empty. This is really cool, but (1) I am worried about adding too many infix
operators and (2) I am not sure exactly what precedence this operator should
have. If we see people complaining about it being a pain to work with functions
that return maybes, that will be good evidence that we should add `(?)` to the
standard libraries.

The second function is a lot more questionable:

```haskell
unsafe : Maybe a -> a

firstNumber =
    unsafe (head numberList)
```

The `unsafe` function extracts a value or crashes. But why? There are a tiny
set of cases where you *know* it is going to be fine and might want this.
For example, imagine you have a `Dict` and the values are lists. You would
never put an empty list in your dictionary, that would be silly, so you know
you can always get elements of the list. I have seen this a few times
programming in languages like Elm, and the `unsafe` function makes the risks
extremely explicit. You can search through code for any occurances of `unsafe`
and quickly identify any risks. It also is a good sigen of &ldquo;maybe you
should try to say the same thing a different way?&rdquo; In any case, this
feels similar in spirit to [`Debug.crash`][crash] which also makes risks very
obvious.

[crash]: http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash

So for those of you using Elm, please define these functions yourself for now
and tell us how it goes! Do you need them? Are they generally bad? Do you have
some good examples of when they are handy? I don't want to add these things to
the standard libraries lightly, so share your evidence with us!


## Import Syntax

We dramatically reduced the set of default imports in 0.14. This was &ldquo;the
right thing to do&rdquo; but it made our existing import syntax feel a bit
clunky. You needed a pretty big chunk of imports to get even basic programs
running. This release introduces improved syntax that will let you cut a
bunch of lines from your import section. As a brief preview, let&rsquo;s look
at the two extremes of the syntax. First we have the plain old import:

```haskell
import List
```

With this, we can refer to any value in the `List` module as `List.map`
or `List.filter`. Using qualified names like this is recommended, so this
should cover most typical cases. Sometimes you want to go crazy though, so on
the other end of the spectrum, we have a way to choose a shorter prefix with
`as` and a way to directly expose some values with `exposing`.

```haskell
import Html.Attributes as Attr exposing (..)
```

In this case we decided to expose *everything* in `Html.Attributes` so we can
just say things like [`class`][class] and [`href`][href] directly. We also
locally rename the module to `Attr` so if there is ever a name collision, we
can say [`Attr.width`][width] to make it unambiguous. You can read more about
this [here](/learn/Modules.elm).

[class]: http://package.elm-lang.org/packages/evancz/elm-html/latest/Html-Attributes#class
[href]: http://package.elm-lang.org/packages/evancz/elm-html/latest/Html-Attributes#href
[width]: http://package.elm-lang.org/packages/evancz/elm-html/latest/Html-Attributes#width

This seems like a tiny change, but it has made a huge difference in how it
feels to work with imports. I have been really happy with it so far. When you
are upgrading your code, keep an eye out for:

  * Need to add `exposing` keyword
  * Importing the same module on two lines. This can now be reduced to one line.
  * Importing [default modules](https://github.com/elm-lang/core#default-imports).
    They come in by default, so there is no need to explicitly import `Signal`
    or `List` unless you are doing something special. (We are planning to add
    warnings for this in a future release to make this easier!)


## Faster Text Rendering

One of our commercial users, [CircuitHub](https://www.circuithub.com/), has
been using collages to render complex circuits. The performance bottleneck for
them was text rendering, so thanks to [James Smith](https://github.com/jazmit),
we added a simple function that let us render to canvas much more efficiently:

```haskell
Graphics.Collage.text : Text -> Form
```

We get to reuse the whole [`Text`](http://package.elm-lang.org/packages/elm-lang/core/latest/Text)
API but we then render direct to canvas to get much better performance. I am
looking forward to seeing this used in practice!

As part of this change, we moved a few functions out of the `Text` library to
clean up the API. Here is a rough listing of stuff that has moved:

```haskell
module Graphics.Element where

leftAligned : Text -> Element
centered : Text -> Element
rightAligned : Text -> Element

show : a -> Element   -- was Text.asText

...
```

The goal here is to make `Text` an abstract representation that can be rendered
in many different contexts. Sometimes that is with `Graphics.Collage`,
sometime that is with `Graphics.Element`, but that should be handled by *those*
libraries.

Keep an eye out for this when you are upgrading! You will need to mess with any
uses of `leftAligned` to get everything working. In the process of upgrading
this website to Elm 0.15 I found this often reduced the number of imports I
needed by quite a lot, especially in smaller beginner examples that used
`asText`.


## Thank you

More so than normal, this release went through a pretty crazy design and
exploration phase, so I want to give a huge thank you to everyone who was part
of that process. I think we put together a ton of great ideas that will make
their way into Elm soon enough! I also want to apologize about how crazy the
list got during that time, sorry about that!

Thank you to [Elm User Group SF](http://www.meetup.com/Elm-user-group-SF/)
which worked with some pre-release versions of 0.15 to vet the tasks API and
start making some new packages for browser APIs.

"""







