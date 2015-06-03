import Graphics.Element exposing (..)
import Markdown
import Website.Widgets exposing (button)
import Website.Skeleton exposing (skeleton)
import Website.BigTiles as Tile
import Website.ColorScheme as C

import Text
import Window

port title : String
port title = "Install"

main = Signal.map (skeleton "Install" content) Window.dimensions

content outer =
    let center elem =
            container outer (heightOf elem) middle elem
    in
        flow down
        [ center (width (min 600 outer) header)
        , center <|
              flow right
              [ button 220 180 "http://install.elm-lang.org/Elm-Platform-0.15.pkg" "Mac"
              , button 220 180 "http://install.elm-lang.org/Elm-Platform-0.15.exe" "Windows"
              ]
        , center (width (min 600 outer) rest)
        ]


header = Markdown.toElement """

# Install

There are handy installers for Windows and Mac. On other platforms you can
[use the npm installer](https://www.npmjs.com/package/elm) or [build from
source](#build-from-source).

"""


rest = Markdown.toElement """

To upgrade to a newer version of Elm, run the installer again. They safely
overwrite old executables so your machine is in a consistent state.

To uninstall on Mac, run [this script][uninstall].

[uninstall]: https://github.com/elm-lang/elm-platform/blob/master/installers/mac/helper-scripts/uninstall.sh

<br>

## Syntax Highlighting

There are quite a few editors that support Elm syntax. Some even integrate with
the REPL. Here are some links to show how to get Elm set up with a couple
different editors:

  * [Sublime Text](https://github.com/deadfoxygrandpa/Elm.tmLanguage)
  * [Atom](https://atom.io/packages/language-elm)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [Vim](https://github.com/lambdatoast/elm.vim)

This plus [Elm Reactor](/blog/Introducing-Elm-Reactor.elm) and you can get a
pretty solid workflow going!

<br>

<span id="build-from-source"></span>
## Build from Source

Elm is written in Haskell; to build it from source, you'll need to get the
Haskell compiler, but you can uninstall that after you have the executables
you need.

On Linux, you'll also need development versions of the C libraries `zlib` and
`tinfo` to build Elm. You can install these on Debian-based systems, such as
Ubuntu, with `sudo apt-get install libz-dev libtinfo-dev`.

**If you have never used Haskell**, first download the [Haskell Platform][hp].
Then run the following commands:

```bash
cabal update
cabal install cabal-install
cabal install -j elm-compiler-0.15 elm-package-0.5 elm-make-0.1.2
cabal install -j elm-repl-0.4.1 elm-reactor-0.3.1
```

This will take some time, but when it is done, all of these executables should
be on your PATH. If not, then they should be in `~/.cabal/bin` which you can
[add to your PATH][add-path].

[hp]: http://hackage.haskell.org/platform/
[add-path]: http://unix.stackexchange.com/questions/26047/how-to-correctly-add-a-path-to-path

**If you use Haskell for other stuff**, it would be best to use cabal
sandboxes for the install process. These were released with cabal 1.18 and will
let you use [this install script][script].

[script]: https://github.com/elm-lang/elm-platform/blob/master/installers/BuildFromSource.hs#L1-L31

<br>

## My First Project

The easiest way to get started with Elm is with the [elm-examples][] project.
It has a couple small Elm programs that are great to learn from and fun to
play with in [Elm Reactor][reactor].

[elm-examples]: https://github.com/evancz/elm-examples
[reactor]: https://github.com/elm-lang/elm-reactor

<br>

## Additional Tools

The Elm Platform comes with quite a few helpful tools in addition to
`elm-reactor`. This section will give a brief overview of them:

  * [`elm-make`](https://github.com/elm-lang/elm-make) &mdash;
    this command line tool actually compiles Elm programs to HTML
    and JavaScript. It is the most general way to compile Elm code, so if your
    project becomes too advanced for `elm-reactor` you may want to start using
    the compiler directly.

  * [`elm-repl`](https://github.com/elm-lang/elm-repl) &mdash;
    REPL stands for [read-eval-print-loop][repl] which lets you play with small
    Elm expressions. The REPL can import code from your projects, so if you want
    to play around with a function burried deep inside a module, you can load it
    into the REPL and test it out. `elm-repl` eventually needs to evaluate
    JavaScript code, so for now you need to install [node.js](http://nodejs.org/)
    to use it.

  * [`elm-package`](https://github.com/elm-lang/elm-package) &mdash;
    this tool lets you grab packages from the [Elm Package
    Catalog](http://package.elm-lang.org/). This is a central home for community
    libraries that solve common problems.

With each of these tools you can use the `--help` flag to get more information.
Each tool also has a README on [GitHub](http://github.com/elm-lang) that has
some helpful information.

  [repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

<br>

## Help

If you are stuck, check to see if anyone has had [a similar issue][elm-platform].
If not, open a new issue or email [the list][group] or ask a question in the
[#elm IRC channel][irc].

[elm-platform]: https://github.com/elm-lang/elm-platform/issues
[group]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss
[irc]: http://webchat.freenode.net/?channels=elm

"""

