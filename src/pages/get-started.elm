import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


port title : String
port title = "Get Started"


main =
  Blog.docs
    "Get Started"
    [ Center.markdown "600px" content ]

content = """

This page will try to guide you through the first steps of using Elm and Elm's toolchain.

We assume you have installed the Elm Platform using one of the methods from the
[Install page][install] on the Elm site.

## Tools

The Elm platform comes with quite a few helpful tools to help you develop Elm programs.
After a successful installation, they should be available on your machine:

- [elm](#elm)
- [elm-package](#elm-package)
- [elm-make](#elm-make)
- [elm-repl](#elm-repl)
- [elm-reactor](#elm-reactor)

Let's go over them one by one:

### elm

`elm` is actually a way to run all other tools.
Try opening a terminal and run `elm` to see the help message.

### elm-package

[elm-package][] is a package managing tool for Elm, making it easy to install and publish packages
to and from the [Elm Package Catalog](http://package.elm-lang.org/).
This is a central home for community libraries that solve common problems.

When starting a new Elm project, run:
```sh
elm package install
```

This will install the `elm-core` package and will create an Elm project file: `elm-package.json`.

In `elm-package.json` you state information on the project, such as
the project name, author, license, dependencies, etc.

#### Notable commands:

- `install`: install the dependencies in `elm-package.json`
- `publish`: publish your library to the Elm Package Catalog
- `bump`: bump version numbers based on API changes
- `diff`: get the difference between two APIs


### elm-make

[elm-make][] is a command line tool used to compile Elm programs to HTML
and JavaScript. It is the most general way to compile Elm code, so if your
project becomes too advanced for `elm-reactor` (see below) you may want to start using
the compiler directly.

When compiling a file (for example: `Main.elm`) into an HTML file (for example: `index.html`),
you probably want to write something like this:

```sh
elm make Main.elm --output=index.html
```

#### Notable flags:

- `--warn`: Prints warnings to improve code quality


### elm-repl


REPL stands for [read-eval-print-loop][repl] which lets you play with small
Elm expressions. the [elm-repl][] can import code from your projects, so if you want
to play around with a function buried deep inside a module, you can load it
into the REPL and test it out. `elm-repl` eventually needs to evaluate
JavaScript code, so for now you need to install [node.js](http://nodejs.org/)
to use it. Since [elm-repl][] only offers a command line interface, browser related functionality
will not work.

#### Notable commands:

- `:help`: will print a help message
- `:exit`: will exit the REPL

### elm-reactor

[elm-reactor][] is an interactive development tool for Elm.
With elm-reactor you can run Elm programs without needing to compile them first.
Also, elm-reactor offers [hot swapping][hs] and [time travel debugging][ttd].

Running `elm reactor` will open a web server on address `0.0.0.0:8000`
where you can visit using a browser and select the file you want to run.
If you want to use elm-reactor's more advanced capabilities,
press the wrench on the left next to the name of the file.
The reactor will open the file and display a column on the right
which offers a way to use those capabilities.

#### Notable flags:

- `-a=<ADDRESS>`: Changes the address at which elm-reactor runs. Since the default address `0.0.0.0` is not supported on browsers such as Chrome and is broadcasting to anyone, we recommend always using `-a=localhost`
- `-p=<PORT>`: Changes the port in which elm-reactor runs.

So for example, run:
```sh
elm reactor -a=localhost
```

Open a browser, and go to `localhost:8000`.

---


With each of these tools you can use the `--help` flag to get more information.

Each tool also has a README on [GitHub](http://github.com/elm-lang) that has
some helpful information.

## Configure Your Editor

We know of Elm syntax highlighting modes for at least the following text editors:

  * [Atom](https://atom.io/packages/language-elm)
  * [Brackets](https://github.com/lepinay/elm-brackets)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [Light Table](https://github.com/rundis/elm-light)
  * [Sublime Text](https://github.com/deadfoxygrandpa/Elm.tmLanguage)
  * [Vim](https://github.com/lambdatoast/elm.vim)
  * [VS Code](https://github.com/sbrink/vscode-elm)

There may be others out there. If you cannot find an Elm mode for your
favorite editor, using Haskell syntax highlighting is close enough to be
usable.

If you do not have an editor at all, Sublime Text is a great one to get
started with.


## Learning Elm

### My First Project

The easiest way to get started with Elm is from the [Examples page](/examples).
It has a few small Elm programs that are great to learn from and fun to
play with in the [Online Editor][OE] or [Elm Reactor][elm-reactor].



### Continue Learning


In the [documentation][] page on the website, you can find a bunch of resources on Elm:

1. For beginners, I recommend starting with the *Elm Complete Guide* ([link to part 1][ECG])
and playing with what you learn
using the `elm-repl` or [Online Editor][OE]. Writing Elm is essential to understanding it, and Elm
provides the tools to easily do so.

- If you prefer video tutorials, check out Pragmatics Studio's Elm tutorial:
[Elm: Building Reactive Web Apps](https://pragmaticstudio.com/elm) with Mike Clark.

- After reading the Elm Complete Guide, you might want to read about Elm's [Syntax][] and [Style Guide][SG].
(an important thing to note is that there are constructs in Elm that are indentation sensitive.)

- To get a better understanding on how to design and build larger Elm programs,
read the [Elm Architecture Tutorial][ea].

- You can also go over the [cs223 Functional Programming course][cs223] by the University of Chicago
which has many good tutorials on Elm and purely functional data structures, but bear in mind
that the course uses `Elm 0.14.1`, which might be a little different from the most updated version.
No worries! That's why you read the complete guide and the syntax guide.

- Read more tutorials like [Elm for the Frontend, Right Now](http://bendyworks.com/elm-frontend-right-now/),  [Checkboard Grid Tutorial](https://github.com/TheSeamau5/elm-checkerboardgrid-tutorial), [Building HTML by Parsing Parameters](http://blog.jessitron.com/2015/08/an-elm-example-reading-url-parameters.html)
and others you can find on [/r/elm](http://reddit.com/r/elm) and the [mailing list][].

- But most importantly, don't forget to **write code**! Here are a few ideas for simple projects to get started with:

    1. Write an Elm program that writes *Right* or *Left* in the middle of the screen depending on whether the mouse cursor is on the left half of the screen or right half of the screen
    2. Write an Elm program that displays dots randomly on the screen, with reset and pause/play buttons
    3. Write an Elm program that displays the name, avatar and list of programming languages
       for a GitHub user entered using a text field
    4. Write an Elm Snake clone (extra: add a highscore)


Additionally: if you ever get stuck, try posting on the [mailing list][] or come to
`#elm` IRC channel on `irc.freenode.net` and ask for help!




[repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
[install]: /install
[Elm]: http://elm-lang.org
[OE]: /try
[documentation]: /docs
[elm-package]: https://github.com/elm-lang/elm-package
[elm-make]: https://github.com/elm-lang/elm-make
[elm-repl]: https://github.com/elm-lang/elm-repl
[elm-reactor]: https://github.com/elm-lang/elm-reactor
[hs]: /blog/interactive-programming
[ttd]: http://debug.elm-lang.org/
[ECG]: /guide/core-language
[cs223]: https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/Home.html
[Syntax]: /docs/syntax
[SG]: /docs/style-guide
[ea]: https://github.com/evancz/elm-architecture-tutorial/
[mailing list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

"""

