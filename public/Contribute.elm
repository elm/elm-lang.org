
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

And remember the [email list][list] and [#elm IRC channel][irc] are great
places to go for help or to talk about ideas.

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "elm-discuss"

<br/>

#### Community

Perhaps surprisingly, the technical and theoretical merits of a project do not have a lot
to do with its success. Building a community and fostering healthy discussion and debate
is vital to a programming language. This stuff is really important!

 * Write about making things in Elm. Learning by example is a core part of Elm so far,
   so help continue the tradition.

 * Hang out on the [#elm IRC channel][irc]. Ask questions, start answering them yourself, and
   talk about how to make Elm better.

 [irc]: http://webchat.freenode.net/?channels=elm "irc"


#### Examples

Build demos for the world to see. Blog about it. Tell people how you did it and
why that is cool. To get started with this, see [how to make Pong][pong]. Some
cool things might be:

  * Diagram-heavy presentations, like [this](/learn/What-is-FRP.elm) and
    [this](physics). Presentations about physics, chemistry, and engineering
    are super easy in Elm!

  * Games: Maze, Sudoku, Breakout, Side-scroller, etc.

  * Be creative! Do something that is easy in Elm, but hard elsewhere.

 [physics]: https://groups.google.com/forum/?fromgroups=#!searchin/elm-discuss/physics/elm-discuss/90Nuj7KCEfw/l3Do6DMbaAoJ "physics"

The most important thing here is to tell people about your work!

 [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"


#### Libraries

Libraries are a very visible way to make a contribution to core Elm infrastructure.
These are tools that all Elm programmers will use and appreciate.

 * Flexible navigation and sidebar libraries. Make it easy to create a blog.
   The modules [here](https://github.com/evancz/elm-lang.org/tree/master/public/Website)
   are the basic framework for this website. They define the color scheme, navigation,
   and formatting. Expand this. Generalize it. Be creative!

 * Build a purely functional way to do 3D graphics with WebGL. Model it on the API design
   choices made in Elm's [`Graphics`](http://elm-lang.org/docs/Graphics/Element.elm)
   library, specifically the `collage` interface and `Forms`.

 * Basic parser combinator library in the style of Haskell's [Parsec][parsec].
   Web programmers have been trying to parse XML with regular expressions for too long!

 [node]: http://nodejs.org/ "node.js"
 [http]: https://github.com/evancz/Elm/blob/master/core-js/Signal/HTTP.js "http code"
 [parsec]: http://www.haskell.org/haskellwiki/Parsec "parsec"


#### Compiler

If you are interested in types, parsing, compilers, or using Haskell for real projects,
this is the section for you! These are big changes that are challenging from a technical
and conceptual perspective. Please try to leave the code with better style and documentation
than when you found it!

 * Figure out how to distribute Elm without Haskell! I imagine having to install the Haskell
   Platform is a barrier for some (many?) users.

 * Figure out how to easily deploy an Elm server to an existing VPS service such as
   Rackspace or AWS. Maybe this just means setting up a OS image that has Elm already
   set up. Maybe something more.

 * Work towards organizing the compiler to make it easier to optimize Elm and to
   compile to different backends (iOS, Android, etc.). This paper on the [design of
   GHC][ghc] may be a good starting point.

 * Work on different compiler backends for iOS, Android, etc. This will include additions
   to the compiler and creating a new runtime system (RTS) for the platform you are targeting.

The compiler code lives [here][compiler], and is roughly divided into sections by the major
directories. If you want to make syntactic or semantic changes, talk to [Evan][evan] early on
to make sure that your ideas fit with the long term vision of Elm.

 [compiler]: https://github.com/evancz/Elm/tree/master/elm "elm compiler"
 [libs]: https://github.com/evancz/Elm/tree/master/core-elm "libraries"
 [infer]: http://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html "Typing Haskell"
 [ghc]: http://community.haskell.org/~simonmar/papers/aos.pdf
 [evan]: https://github.com/evancz "Evan"


|]

main = lift (skeleton blog) Window.dimensions

