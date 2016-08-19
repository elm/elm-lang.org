import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton



main =
  Skeleton.skeleton
    "docs"
    [ Center.markdown "600px" content
    ]


content = """

# Documentation

### Quick Overview

  * **[Get Started](/get-started)**
  * [Examples](/examples)
  * [The Elm Architecture](http://guide.elm-lang.org/architecture/)
  * [Let's be Mainstream!](http://www.elmbark.com/2016/03/16/mainstream-elm-user-focused-design)


### Learn Elm

  * **[An Introduction to Elm](http://guide.elm-lang.org)**
  * [FAQ](http://elm-community.github.io/elm-faq/)
  * [Syntax](/docs/syntax)
  * [Syntax vs JS](/docs/from-javascript)
  * [Style Guide](/docs/style-guide)
  * [Package Design](http://package.elm-lang.org/help/design-guidelines)
  * [Writing Documentation](http://package.elm-lang.org/help/documentation-format)
  * [Advanced Topics](/docs/advanced-topics)


### Packages

  * **[All Community Packages](http://package.elm-lang.org)**
  * [Core](http://package.elm-lang.org/packages/elm-lang/core/latest/)
  * [HTML](http://package.elm-lang.org/packages/elm-lang/html/latest/)


"""
