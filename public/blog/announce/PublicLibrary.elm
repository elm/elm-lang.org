
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.10.1")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  width w intro

intro = [markdown|

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

<h1><div style="text-align:center">Elm Public Library
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Discover libraries, browse documentation*</div></div>
</h1>

Elm has a basic package manager now, so you can easily discover and
use other people's libraries. This comes in two parts:

  1. [Elm Public Library](http://library.elm-lang.org/) which lets you
     discover libraries and browse their documentation.

  2. The `elm-get` command line tool which lets you install libraries
     you find in the [Elm Public Library](http://library.elm-lang.org/)
     and publish your own.

Once you install `elm-get` with `cabal install elm-get`, you will be all
set to start using any library in [the catalog](http://library.elm-lang.org/catalog)
in your projects.

## Installing Libraries

    elm-get install evancz/automaton

## Publishing Libraries

The first step of publishing a library is writing one in the first place!
Writing good libraries is hard, so definitely take a look at [the API design
guidelines]() before getting started. The guidelines are a set of
best-practices that hopefully will help people create great libraries.

Okay, once you have written a library, it is time to share it with the world.
This section will go through how I published the `evancz/automaton` library,
showing how to use `elm-get`.

The Public Library is backed by GitHub. Every registered project
is tied to a specific repo on GitHub. This means you can star projects you
like, browse the source code, open issues if you find bugs, or look at the
other projects that author has created. So the first thing you must do
to publish a library is create a GitHub repo for your project.

Specific releases are indicated by tagging the master branch of your
repo. When you add a tag called `0.1`, it becomes possible to publish
version 0.1 of your library. This ensures that each release is always
tied to a specific commit.

To make a release you must describe your project in the `elm_dependencies.json` file.

You must also document the libraries you wish to expose publicly. The
documentation format is described [here]().

As of today, published libraries cannot have 3rd party dependencies.
More on why in the next section.

## Current Limitations and Future Plans

For the first release, you can only publish libraries that have no
3rd party dependencies. You can rely on the standard libraries, but
not community libraries. This is a serious limitation, but this means
people can start sharing a certain class of libraries today. I think
this tradeoff is reasonable for the first version for a couple reasons:

  * Some sharing is better than no sharing.

  * The first batch of libraries have no 3rd party libraries to depend on.

  * There is a non-trivial set of libraries that do not need 3rd party dependencies.

Package managers are very difficult to do well, specifically when
it comes to managing complex dependencies in a project. Doing it wrong
would be really unfortunate for everyone using Elm, so I am being
extremely cautious. I want the tools surrounding Elm to be really nice to use!

# Thank you

Thank you to everyone who attended or spoke at the [Elm Workshop](/blog/announce/Workshop-2013.elm)
in Budapest! I had a lot of fun and the talks and projects were great. I came away
with some very good ideas about how to improve the Elm runtime, and more immediately,
seeing everyones work was really inspiring and really helped me pick up the pace on
this project! More specifically, thank you to Mads for recommending a strategy that
let me get this release out sooner.

Thank you to [Irakli](https://twitter.com/gozala) for reviewing `elm-get` with me to
make sure it can grow into a nice package manager.
Thank you to [Joe Collard](https://github.com/jcollard/) for working on
[elm-mode](https://github.com/jcollard/elm-mode) for emacs and starting on integration
with `elm-get`.
Thank you to everyone who has already submitted libraries to the Public Library!

|]
