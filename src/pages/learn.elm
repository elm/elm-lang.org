import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


port title : String
port title = "Learning Elm"


main =
  Blog.docs
    "Learning Elm"
    [ Center.markdown "600px" content ]

content = """

The easiest way to get started with Elm is from the [Examples page](/examples).
It has a few small Elm programs that are great to learn from and fun to
play with in the [Online Editor][OE] or, if you have [installed Elm][install],
in the [Elm Reactor][elm-reactor].

## Continue Learning

In the [documentation][] page on the website, you can find a bunch of resources on Elm:

1. For beginners, we recommend starting with the *Elm Complete Guide* ([link to part 1][ECG])
and playing with what you learn
using the [elm-repl][] or [Online Editor][OE]. Writing Elm is essential to understanding it, and Elm
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

    1. Write an Elm program that writes *Right* or *Left* in the middle of the screen depending on whether the mouse cursor is on the left half of the screen or right half of the screen.
    2. Write an Elm program that displays dots randomly on the screen, with reset and pause/play buttons.
    3. Write an Elm program that displays the name, avatar and list of programming languages
       for a GitHub user entered using a text field.
    4. Write an Elm Snake clone (extra: add a highscore).

Additionally: if you ever get stuck, try posting on the [mailing list][] or come to
`#elm` IRC channel on `irc.freenode.net` and ask for help!

[install]: /install
[OE]: /try
[documentation]: /docs
[elm-repl]: https://github.com/elm-lang/elm-repl
[elm-reactor]: https://github.com/elm-lang/elm-reactor
[ECG]: /guide/core-language
[cs223]: https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/Home.html
[Syntax]: /docs/syntax
[SG]: /docs/style-guide
[ea]: https://github.com/evancz/elm-architecture-tutorial/
[mailing list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

"""

