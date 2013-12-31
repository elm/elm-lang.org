
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm Package Manager")
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

<h1><div style="text-align:center">Elm Package Manager
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Making it easy to share code*</div></div>
</h1>

Elm now has a basic package manager, so you can easily discover and
use community libraries. This comes in two parts:

  1. [Elm Public Library](http://library.elm-lang.org/) which has
     a catalog of all published libraries along with their documentation

  2. [`elm-get` command line tool](https://github.com/evancz/elm-get#elm-get)
     for installing and publishing libraries

Once you install `elm-get` with `cabal install elm-get`, you will be all
set to start using any library in [the catalog](http://library.elm-lang.org/catalog)
in your projects!

For more information on how to use `elm-get` see [the usage
instructions](https://github.com/evancz/elm-get#elm-get). The rest of this post
focuses on why this project is important for Elm, the long-term goals, and a
rough roadmap for future releases.

## Libraries are wildly important

Upstart languages live and die by their libraries. Ruby and
[PHP](http://www.php.net/manual/en/history.php.php#history.php3)
are obvious examples of this. Scala and Clojure are great examples
of this too, stressing the tremendous value of a good foreign
function interface. OCaml and Haskell make a good case study
for Elm.

OCaml is a very nice language, with the same theoretical and syntactic
roots as languages like Haskell and Elm. OCaml strongly encourages a
purely functional style but has easy-to-use safety valves for ducking
into imperative or object-oriented styles when necessary. It's very similar to the
formula that has made Scala attractive for JVM users. So why is Haskell&mdash;a
more ideologicly extreme language&mdash;gaining more traction these days?

I think the primary factor is that [until 2013](http://opam.ocaml.org/),
OCaml did not have a way to share libraries. User would inevitably face
a problem that was not solved by any existing library, so they would
need to start their own from scratch. Without collaboration and sharing,
the start up costs to using OCaml remained constant. Haskell made library
sharing possible as early as [2003](http://hackage.haskell.org/package/Cabal)
or [2005](https://web.archive.org/web/*/hackage.haskell.org). The set of
shared libraries slowly expanded, building on top of itself. Over time, the
start up costs to using Haskell got lower.

Obviously there are other important and interesting differences between OCaml
and Haskell, but I do not believe that any of them are nearly as important when
it comes to industry adoption.

## Goals of the Elm Public Library

Knowing that libraries are wildly important, I looked around at what was
already out there. Ideally I could rely on something that
already exists and happens to be a great fit for Elm. Two commonly suggested
choices were [npm](https://npmjs.org/) and [Nix](http://nixos.org/nix/).
Both sounded plausible, but I decided to create a set of goals before
choosing an approach. I wanted package management in Elm to:

 * **Encourage collaboration and experimentation, make sharing easy.**
   Purely functional GUIs is not a well-explored design space, so it
   is important to experiment with layouting, color scheming, form
   construction, graphing, data visualization, etc. You can go too
   far with this though. Having 9 terrible libraries is
   worse than having none. Now you have to try them all *before*
   writing your own. That brings us to...

 * **Guide authors towards high quality APIs and implementations.**
   The first step is having a basic set of [design guidelines](http://library.elm-lang.org/DesignGuidelines.html)
   to have some level of consistency across projects. Social tools can push
   quality higher too, like clearly associating projects with authors.

 * **Make it easy to use.** Having nicely formatted documentation for all
   libraries is required. The command line tools should work on all major
   platforms. The data collected for this site should be available for improving
   tooling, like [function search](http://www.haskell.org/hoogle/) or
   general autocomplete in editors. But most importantly, it should make
   every effort to avoid dependency hell!

The deciding factor was that I want to have nicely formatted documentation for
all libraries. To make this easy, there needs to be some central site to host
everything. Once you have a tool to upload metadata about packages, relying
on something else to work out dependencies seems much less attractive. Users
have an extra program to install and learn, it may have many irrelevant or
problematic features, and there are extra steps to publish things. With the
specific needs of each language and the fact that dependency management is
unsolved to a certain extent, it made a lot of sense that this is a problem
that new languages tend to tackle for themselves. I was not pleased to reach
this conclusion, but on close inspection, it seems like the only way that works
for the *users* of a language.

Those goals bring us to the roadmap for [`elm-get`](https://github.com/evancz/elm-get#elm-get)
and [the Elm Public Library](http://library.elm-lang.org/).

## Roadmap

**Versioning:** There are some checks on versioning. Right now, it ensures that
you are always publishing higher numbers. Eventually, I'd like to enforce [semantic
versioning](http://semver.org/) by actually comparing APIs between versions and
finding type changes and additional values.

**Dependencies:** The initial release does not permit dependencies except on the
standard library. That simplifies my task for now, getting this release out earlier.
The first batch of libraries won't have other dependencies anyway. (Thank you Mads
for this idea!) Lifting this restriction is the highest priority for this project,
and the ultimate goal is to allow multiple versions of a library in the same project
(trying to avoid [cabal hell][cabal]). I *suspect* breaking changes will be necessary
as I pursue this goal and see what is needed for Elm.

 [cabal]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html#what-are-sandboxes-and-why-are-they-needed

**Avoiding duplication:** Dependencies will be "flat" so if two libraries depend
on `evancz/automaton` version 0.1, they always share that code
([like Nix](http://nixos.org/nix/),
[unlike in npm](https://github.com/Gozala/method/wiki/Known-Issues#dedup-by-default)).
It may be possible to increase sharing of libraries in an semi-automated way. One could
look at all of the functions you use from a library and then find all of the versions of that
library where those specific functions exist and have the same type. This could give
an estimated maximal dependency range. Unfortunately, the meaning of a function may
change even if its type does not, but this at least trims down the possibilies. Pairing
this strategy with the test suite for a library could further trim things down.

**Rating / evaluating libraries:** All libraries need to live on github right now, so
their star system and issue tracker can be good resources on how many people use a
library and what problems may come up. It'll also be pretty clear who is responsible
for libraries. I think this may be a good marker. Library authors get more recognition,
users can figure out who they trust to make solid libraries and good APIs, etc.
I really want to provide more usage data than this though. Some possibly crazy
ideas include: tracking *unique* downloads, tracking version usage so you know
if people are upgrading or not, tracking function usage so you know the true
impact of a breaking change. It is unclear if any of this is a good idea for a
bunch of reasons, but I imagine it'd help library authors do a better job.

**Extensions:** All of the info used to run this is just JSON data. It is currently
possible to download any of it from the site directly, giving you access to [a list
of every library](http://library.elm-lang.org/libraries.json) and [docs for each
library](http://library.elm-lang.org/catalog/evancz-automaton/0.1/docs.json).
This is great if you want to work on search tool like
[Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) or
[Hoogle](http://www.haskell.org/hoogle/) for Elm (Elm Library Search? Elmoogle?)
or try to work on a strategy for estimating maximal dependency ranges based on
[`elm-doc` data](http://library.elm-lang.org/catalog/evancz-automaton/0.1/docs.json).

# Thank you

Thank you to everyone who attended or spoke at the [Elm Workshop](/blog/announce/Workshop-2013.elm)
in Budapest! (We are working on getting the videos out!) I had a lot of fun and the talks
and projects were great. I came away with some very good ideas about how to improve the
Elm runtime, and more immediately, seeing everyones work was really inspiring and really
helped me pick up the pace on this project!

Thank you to [Irakli](https://twitter.com/gozala) for reviewing `elm-get` with me to
make sure it can grow into a nice package manager.
Thank you to [Joe Collard](https://github.com/jcollard/) for working on
[elm-mode](https://github.com/jcollard/elm-mode) for emacs and starting on integration
with `elm-get`.
And thank you to everyone who has already published libraries!

|]
