import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Website.Widgets (button)
import String
import Window

main = skeleton "Libraries" content <~ Window.dimensions

content outer =
    let inner = 600
        half = inner // 2
        center elem =
            container outer (heightOf elem) middle elem
        centerText msg =
            let msg' = width inner msg
            in  center msg'
    in
      flow down
      [ centerText intro
      , center (button outer 320 "http://package.elm-lang.org/packages/elm-lang/core/latest/" "Standard Libraries")
      , centerText midtro
      , center (button outer 320 "http://package.elm-lang.org/packages" "Community Libraries")
      , centerText outro
      ]

intro = Markdown.toElement """

# Libraries

Documentation for all Elm libraries can be found at
[package.elm-lang.org](http://package.elm-lang.org).

The Standard Libraries come with the latest release of the Elm compiler and
make it easy to get productive.

"""

midtro = Markdown.toElement """

If you cannot find it in the Standard Libraries, the Elm community is probably
working on it!

"""

outro = Markdown.toElement """

See the [syntax reference](/learn/Syntax.elm) and [other learning
resources](/Learn.elm) to learn more about the language itself.

## Search

Every page on [package.elm-lang.org](http://package.elm-lang.org) has a search
box that lets you filter through results. For example, in the standard
libraries, you can search the documentation for functions like `map` and
`length` or operators like `/=` and `|>`.

## Written in Elm

Just like [this website](https://github.com/elm-lang/elm-lang.org), the Public
Library is also written entirely in Elm. Check out [the source
code](https://github.com/elm-lang/elm-package) to see how FRP makes live search
easy to implement.

"""
