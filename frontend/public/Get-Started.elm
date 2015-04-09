import Graphics.Element exposing (..)
import Markdown

import Website.Skeleton exposing (skeleton)
import Window

port title : String
port title = "Successful Install!"

main = Signal.map (skeleton "" everything) Window.dimensions

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w pageTitle
  , section intro
  ]

pageTitle = Markdown.toElement """
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Success!</div>
<div style="font-size: 1.5em;">You just installed Elm, now what?</div>
</div>
"""

intro = Markdown.toElement """

<br>

You have installed the Elm Platform, so you now have a bunch of helpful
command line tools to help you develop Elm programs. This tutorial will teach
you how to use them!

## My First Project

The easiest way to get started with Elm is with the [elm-examples][] project.
It has a couple small Elm programs that are great to learn from and fun to
play with in [Elm Reactor][reactor].

[elm-examples]: https://github.com/evancz/elm-examples
[reactor]: https://github.com/elm-lang/elm-reactor

## Continue Learning

Navigate to the [Learn](/Learn.elm) and [Examples](/Examples.elm) pages to
read tutorials and see tons of example programs. Try creating files in your
project and running them on your computer.

## Configure Your Editor

We know of Elm syntax highlighting modes for at least three text editors:

  * [Sublime Text](https://github.com/deadfoxygrandpa/Elm.tmLanguage)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [Vim](https://github.com/lambdatoast/elm.vim)

There may be others out there. If you cannot find an Elm mode for your
favorite editor, using Haskell syntax highlighting is close enough to be
usable.

If you do not have an editor at all, Sublime Text is a great one to get
started with.

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

"""
