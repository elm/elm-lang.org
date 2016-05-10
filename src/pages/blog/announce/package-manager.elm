import Blog
import Center


main =
  Blog.blog
    "Elm Package Manager"
    "Making it easy to share code"
    Blog.evan
    (Blog.Date 2014 1 1)
    [ Center.markdown "600px" content ]


content = """

Elm now has a basic package manager, so you can easily discover and
use community libraries. This comes in two parts:

  1. [Elm Public Library](http://package.elm-lang.org/) which has
     a catalog of all published libraries along with their documentation

  2. [`elm-get` command line tool](https://github.com/elm-lang/elm-get#elm-get)
     for installing and publishing libraries

[The recent 0.10.1 compiler release](/blog/announce/0.10.1) introduced
integration with `elm-get` to make things easy to use. So once you [install
`elm-get`](https://github.com/elm-lang/elm-get#install) with `cabal install elm-get`,
you will be all set to start using any library in
[the catalog](http://package.elm-lang.org/) in your projects!

For more information on how to use `elm-get` see [the usage
instructions](https://github.com/elm-lang/elm-get#elm-get). The rest of this post
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
   The first step is having a basic set of [design guidelines](http://package.elm-lang.org/help/design-guidelines)
   to have some level of consistency across projects. Social tools can push
   quality higher too, like clearly associating projects with authors.

 * **Make it easy to use.** Having nicely formatted documentation for all
   libraries is required. The command line tools should work on all major
   platforms. The data collected for this site should be available for improving
   tooling, like [function search](http://www.haskell.org/hoogle/) or
   general autocomplete in editors. But most importantly, it should make
   every effort to avoid dependency hell!

No available option was a great fit for this, specifically on the third goal.
Nix does not work on Windows, and its expressiveness makes it quite complex
for the simple set of things that Elm needs. npm would make discoverability
really tough. And crucially, neither provide a way to have nicely formatted
documentation for all libraries. So no matter what option I chose, I would
still need a way to upload metadata to a central repo to have
[a catalog](http://package.elm-lang.org/) with documentation.

I decided to take a route inspired by Go and [OCaml](http://opam.ocaml.org/)
(of course without the versioning issues with `go get`). The [Elm Public
Library](http://package.elm-lang.org/) is backed by GitHub, which
covers a decent amount of functionality, but leaves the more language
specific tasks to me. This makes it easy to display documentation,
work on discoverability, and integrate with the compiler and tools
in a way that is best for Elm while still offloading as much work as
possible.

I would be very happy if there was a way for me to do less work and
still provide users with a great experience, but after researching
and looking at the concrete details of different alternatives,
I think the route I chose is ultimately best for the *users* of a language
and minimizes the amount of work needed.

## Roadmap

There is still a lot of work to do on
[`elm-get`](https://github.com/elm-lang/elm-get#elm-get)
and [the Elm Public Library](http://package.elm-lang.org/), so the roadmap
breaks up into fairly distinct topics.

**Versioning:** Right now, `elm-get` ensures that you are always publishing
higher numbers. Eventually, I'd like to automatically enforce [semantic
versioning](http://semver.org/) by actually comparing APIs between versions and
finding type changes and additional values.

**Dependencies:** The initial release does not permit dependencies except on the
standard library. That simplifies my task for now, getting this release out earlier.
The first batch of libraries won't have other dependencies anyway. (Thank you Mads
for this idea!) Lifting this restriction is the highest priority for this project,
and the ultimate goal is to allow multiple versions of a library in the same project
(trying to avoid [cabal hell][cabal]).

 [cabal]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html#what-are-sandboxes-and-why-are-they-needed

**Avoiding duplication:** Dependencies will be "flat" so if two libraries depend
on `evancz/automaton` version 0.1, they always share that code
([like Nix](http://nixos.org/nix/),
[unlike in npm](https://github.com/Gozala/method/wiki/Known-Issues#dedup-by-default)).
To further increase sharing, it is possible to estimate a maximal
dependency ranges. If your project uses functions `List.map` and
`List.foldl`, the maximal dependency range would be all versions of `List` where
the types of `map` and `foldl` are unchanged. Unfortunately, the *meaning*
of a function may change even if its type does not, so this is an estimate at best.
Pairing this strategy with the test suite for a library could further trim things down.

**Data for users and authors:** I think there is tons of data that package
managers usually do not track, yet could be very valuable for users. Ideas
include: tracking *unique* downloads, tracking version usage so you know
if people are upgrading or not, tracking function usage so you know the true
impact of a breaking change. It is unclear if any of this is a good idea for a
variety of reasons, but I imagine it'd help library authors do a better job.

**Extensions:** All of the info used to run the Public Library is just JSON data.
It is currently possible to download any of it from the site directly, giving you
access to [a list of every uploaded library](http://package.elm-lang.org/all-packages)
and [docs for each version of each
library](http://package.elm-lang.org/packages/evancz/automaton/latest/documentation.json).
This is great if you want to work on search tool like
[Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) or
[Hoogle](http://www.haskell.org/hoogle/), implement autocomplete in an editor,
or estimate maximal dependency ranges based on
[`elm-doc` data](http://package.elm-lang.org/packages/evancz/automaton/latest/documentation.json).

There is a ton of work, so check out [the repo](https://github.com/elm-lang/elm-get)
if you are interested in contributing! I am slowly becoming better at managing
projects and I am doing my best to make it easy and fun to contribute.

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

"""
