import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


main =
  Blog.docs
    "Get Started"
    [ Center.markdown "600px" content ]


content = """

This page will try to guide you through the first steps of using Elm.

First, make sure you [**install Elm**](/install)!


# Tools

After a successful installation, the following command-line tools should be
available on your computer:

- [`elm`](#elm)
- [`elm-package`](#elm-package)
- [`elm-make`](#elm-make)
- [`elm-repl`](#elm-repl)
- [`elm-reactor`](#elm-reactor)

Each one has a `--help` flag that will show more information. Let's go over
them here though!


## elm

The `elm` command is actually a way to run all other tools.
Try opening a terminal and run `elm` to see the help message.


## elm-package

The [`elm-package`](https://github.com/elm-lang/elm-package) command helps you
download and publish packages from our
[package catalog](http://package.elm-lang.org/). As community members solve
problems [in a nice way](http://package.elm-lang.org/help/design-guidelines),
they share their code in the package catalog for anyone to use!

When starting a new Elm project, run:

```bash
elm-package install
```

This will create an `elm-package.json` file that describes your project. Most
importantly, it lists the packages you depend on. By default, this includes
`elm-lang/core` and `elm-lang/html` so you get all the basic stuff you need to
get started.

**Notable commands:**

- `install`: install the dependencies in `elm-package.json`
- `publish`: publish your library to the Elm Package Catalog
- `bump`: bump version numbers based on API changes
- `diff`: get the difference between two APIs


## elm-make

The [`elm-make`](https://github.com/elm-lang/elm-make) command is for building
Elm projects. It can compile Elm code to HTML or JavaScript. It is the most
general way to compile Elm code, so if your project becomes too advanced for
`elm-reactor` ([see below](#elm-reactor)) you will want to start using
the compiler directly.

Say we create an Elm file called `Main.elm` and want to compile it to `main.html`.
You would run this command:

```bash
elm-make Main.elm --output=main.html
```

**Notable flags:**

- `--warn` prints warnings to improve code quality


## elm-repl

The [`elm-repl`](https://github.com/elm-lang/elm-repl) command opens up a
[read-eval-print-loop][repl] (REPL) for working with Elm expressions. If you
decide to work through [An Introduction to Elm](http://guide.elm-lang.org/)
you will start out working in the REPL!

[repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

`elm-repl` can import any module in your project. So if you want to work with
functions from [`String`](http://package.elm-lang.org/packages/elm-lang/core/latest/String)
module, you would do something like this:

```bash
elm-repl
---- elm-repl 0.17.1 -----------------------------------------------------------
 :help for help, :exit to exit, more at <https://github.com/elm-lang/elm-repl>
--------------------------------------------------------------------------------
> import String
> String.reverse "hello"
"olleh" : String
```

You can import *your* modules too, so if you create a module named `MyThing` in
your project, you can say `import MyThing` in the REPL and get access to all the
values it exposes.

`elm-repl` compiles expressions to JavaScript for now, so you need
[Node.js](http://nodejs.org/) installed to get it working. Since `elm-repl`
only offers a command line interface, browser related functionality will not
work.


**Notable commands:**

- `:help`: will print a help message
- `:exit`: will exit the REPL


## elm-reactor

The [`elm-reactor`](https://github.com/elm-lang/elm-repl) command is like a
nicer version of `elm-make`, helping you build Elm projects without messing
with the command-line.

To use it, run the following command at the root of your Elm project:

```bash
elm-reactor
```

This starts a server at [`http://localhost:8000`](http://localhost:8000). Check
it out! It lets you navigate through your project, and when you select an
`.elm` file, it will compile it for you behind the scenes.

**Notable flags:**

- `--port` lets you pick something besides port 8000. So you can say
  `elm-reactor --port=8123` to get things to run at `http://localhost:8123`.
- `--address` lets you replace `localhost` with some other address. For
  example, you may want to use `elm-reactor --address=0.0.0.0` if you want to
  try out an Elm program on a mobile divice through your local network.


# Configure Your Editor

We know of Elm syntax highlighting modes for at least the following text editors:

  * [Atom](https://atom.io/packages/language-elm)
  * [Brackets](https://github.com/lepinay/elm-brackets)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [IntelliJ](https://github.com/durkiewicz/elm-plugin)
  * [Light Table](https://github.com/rundis/elm-light)
  * [Sublime Text](https://packagecontrol.io/packages/Elm%20Language%20Support)
  * [Vim](https://github.com/lambdatoast/elm.vim)
  * [VS Code](https://github.com/sbrink/vscode-elm)

If you do not have an editor at all, [Sublime Text](https://www.sublimetext.com/)
is a great one to get started with.


# Learning Elm

## My First Project

The easiest way to get started with Elm is from the [Examples page](/examples).
It has a few small Elm programs that are great to learn from and fun to
play with in the [online editor](/try) or `elm-reactor`.


## Continue Learning

The [documentation](/docs) page has a bunch of resources on Elm:

  - **Official Guide** &mdash; A great way to get started is with
  [An Introduction to Elm](http://guide.elm-lang.org/). Make sure you are using
  `elm-repl` or the [online editor](/try) to follow along! Writing code is the
  best way to understand Elm!

  - **Syntax / Style** &mdash; After reading through the guide, you might want
  to check out the [syntax](/docs/syntax) and [style guide](/docs/style-guide).

  - **Bigger Programs** &mdash; To get a better understanding on how to build
  large Elm programs, focus on [The Elm Architecture](http://guide.elm-lang.org/architecture/).
  There is some great advice in [this thread](https://groups.google.com/forum/?fromgroups#!topic/elm-discuss/_cfOu88oCx4)
  from Richard Feldman, and videos like [this API design session](https://www.youtube.com/watch?v=KSuCYUqY058)
  show the thinking behind projects like [elm-sortable-tables][] and
  [elm-autocomplete][] that are nice examples of how to reuse view code.

[elm-sortable-tables]: https://github.com/evancz/elm-sortable-table
[elm-autocomplete]: https://github.com/thebritican/elm-autocomplete

  - **Community** &mdash; The fastest way to learn is to talk with other people in
  the Elm community! For example, if you ever get stuck on something, go to [the
  Elm Slack](http://elmlang.herokuapp.com/) and ask about it. Folks are friendly
  and happy to help. You can save yourself hours. Just do it! You can also
  check out [/r/elm](https://www.reddit.com/r/elm) or [@elmlang](https://twitter.com/elmlang)
  to catch blogs and projects that people are working on.

  - **Write code!** &mdash; There is no substitute for experience! Find a small
  project to start with and work on it. Maybe that is a game of snake. Maybe it
  means rewriting a project you did in Angular or React. Just make sure you are
  building something!

"""
