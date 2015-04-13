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

So as of today, we have a new library called [elm-http][] that makes it really
pleasant to grab JSON and strings from servers, but in the next few months the
community is going to be filling in a lot of gaps by creating libraries for
APIs like local storage, geolocation, dropbox.js, firebase, etc. etc. Easy
interop is super important to making people productive, so this is a huge step
for Elm.

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
and manages errors. The programmer creates a task, and hands it to Elm&rsquo;s
[runtime system][rts] to do all the dirty work. Think of handing a list of
tasks to a personal assistant, and they just go off and work on it. Maybe they
will fail, maybe they will succeed. You just hear about it when they are done.
This lets us keep all the benefits that make Elm great, but add a lot of
expressive power.

So how do these tasks work? As a simple example, let’s get the README for
Elm's core libraries.

```elm
import Http

getReadme : Task Http.Error String
getReadme =
    Http.getString "http://package.elm-lang.org/packages/elm-lang/core/latest/README.md"
```

So `getReadme` is a `Task` that we can give to the runtime. When the runtime
gets a hold of it, the task will either fail with an [`Http.Error`][error] or
succeed with a string of markdown.

[error]: http://package.elm-lang.org/packages/evancz/elm-http/latest/Http#Error

To learn more about tasks, check out [the tutorial](/learn/Tasks.elm)!


## Imports

We dramatically reduced the set of values imported by default in 0.14. This was
&ldquo;the right thing to do&rdquo; but it made our existing import syntax feel
a bit clunky. This release introduces improved syntax that will let you cut a
bunch of lines from your import section. As a brief preview, here are the two
extremes of the syntax:

```elm
import List

import Html.Attributes as Attr exposing (..)
```

In the first case, we can refer to any value in the `List` module as `List.map`
or `List.filter`. This should cover a ton of typical cases.

Sometimes you want to go a bit crazier though, so in the second case we have a
way to choose a shorter prefix and a way to directly expose some values. We
decided to expose *everything* in `Html.Attributes` so we can just say things
like `class` and `href` directly. We also expose `Attr` so if there is ever a
name collision, we can say `Attr.width` to make it unambiguous. You can read
more about this [here](/learn/Modules.elm).

This seems like a tiny change, but it feels really nice. When you are upgrading
keep an eye out for:

  * Need to add `exposing` keyword
  * Importing the same module on two lines. This can be reduced to one line.
  * Importing [default modules](https://github.com/elm-lang/core#default-imports).
    They come in by default, no need to import `Signal` or `List` twice!


## Faster Text Rendering

One of our commercial users, [CircuitHub](https://www.circuithub.com/), has
been using collages to render complex circuits. The performance bottleneck for
them was text rendering, so thanks to [James Smith](https://github.com/jazmit),
we added a simple function that let us render to canvas much more efficiently:

```elm
text : Text -> Form
```

We get to reuse the whole [`Text`](http://package.elm-lang.org/packages/elm-lang/core/latest/Text)
API but get a lot better performance. Looking forward to seeing this used in
practice!


##

"""







