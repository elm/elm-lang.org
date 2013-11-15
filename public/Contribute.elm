import Website.Skeleton (skeleton)
import Window
import JavaScript as JS

title = constant (JS.fromString "Contribute to Elm")
foreign export jsevent "elm_title"
  title : Signal JS.JSString

blog w = width w [markdown|

# Contribute

Elm needs your support! And there are tons of projects to work on. Major
areas to contrubute to are:

 * [Community](#community)
 * [Examples](#examples)
 * [Libraries](#libraries)
 * [Compiler](#compiler)

If you decide to take on a project, be sure to mention this on the [email list][list]
so that you can get early feedback from the people who will eventually be reviewing
and using your code. This will also help avoid duplication of effort.

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "elm-discuss"

#### Community

Building a community and fostering healthy discussion and debate
is vital to a programming language. This is an easy an important way to help.

 * **Blog** &ndash; Write about making things in Elm, and share in
   [/r/programming](http://reddit.com/r/elm) or [/r/elm](http://reddit.com/r/elm).
   Learning by example is a core part of Elm, so help continue the tradition.

 * **IRC** &ndash; Hang out on the [#elm IRC channel][irc]. Ask questions, start answering
   them yourself, and talk about how to make Elm better. Make sure discussions
   are friendly and evidence-based.

 [irc]: http://webchat.freenode.net/?channels=elm "irc"

#### Examples

Build demos for the world to see. Blog about it. Tell people how you did it and
why that is cool. To get started with this, see [how to make
Pong](/blog/games-in-elm/part-0/Making-Pong.html). Some cool things might be:

  * **Diagrams** &ndash; like [this](/learn/What-is-FRP.elm) and
    [this](/examples/Intermediate/Physics.elm). Interactive presentations
    about programming, physics, chemistry, and engineering are super easy in Elm!

  * **Games** &ndash; such as Sudoku,
    [Breakout](https://github.com/Dobiasd/Breakout#breakout--play-it),
    a [side-scroller](/examples/Intermediate/Mario.elm),
    [an RPG](/examples/Intermediate/Walk.elm),
    a [maze](https://github.com/Dobiasd/Maze#maze--play-it),
    space invaders, etc.

  * **Data Visualization** &ndash; like [this pie
    chart](http://www.share-elm.com/sprout/524bb079e4b0d6a98b152be8). See what
    other kinds of plots and charts are simple in Elm.

#### Libraries

Libraries are a very visible way to make a contribution to core Elm infrastructure.
These are tools that all Elm programmers will use and appreciate.

 * **GUI widgets** &ndash; navigation, sidebars, forms, blog frameworks, etc. These
   are all areas in which a good Elm library could make things much simpler.
   The modules [here](https://github.com/evancz/elm-lang.org/tree/master/public/Website)
   are the basic framework for this website. They define the color scheme, navigation,
   and formatting. Expand this. Generalize it. Be creative!

 * **WebGL** &ndash; purely functional 3D graphics. Model it on the API design
   choices made in Elm's
   [`Graphics.Collage`](http://docs.elm-lang.org/library/Graphics/Collage.elm) library.

 * **Parsing** &ndash; make parsing easy in the style of
   Haskell&rsquo;s [Parsec][parsec]. Web programmers have been trying to parse
   XML with regular expressions for too long!

 [parsec]: http://www.haskell.org/haskellwiki/Parsec "parsec"


#### Compiler

If you are interested in types, parsing, compilers, or using Haskell for real projects,
this is the section for you! These are big changes that are challenging from a technical
and conceptual perspective. Please try to leave the code with better style and documentation
than when you found it!

 * **Distribution** &ndash; figure out how to distribute Elm without Haskell!
   I imagine having to install the Haskell Platform is a barrier for some (many?) users.

 * **Deploy** &ndash; make it easy to deploy an Elm server to AWS, Rackspace, Azure, etc.
   Maybe this just means setting up a OS image that has Elm already set up. Maybe something
   more.

 * **Backends** &ndash; get Elm working on CLR or LLVM. Try compiling through F# or OCaml
   to get Elm running natively on iOS and Android.

The compiler code lives [here][compiler], and is roughly divided into sections by the major
directories. If you want to make syntactic or semantic changes, talk to [Evan][evan] early on
to make sure that your ideas fit with the long term vision of Elm.

 [compiler]: https://github.com/evancz/Elm "elm compiler"
 [libs]: https://github.com/evancz/Elm/tree/master/core-elm "libraries"
 [infer]: http://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html "Typing Haskell"
 [evan]: https://github.com/evancz "Evan"


|]

main = lift (skeleton blog) Window.dimensions
