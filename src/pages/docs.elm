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

### Quick Start

  * [For JS users](/docs/from-javascript)
  * [Make an HTML app](https://github.com/evancz/start-app)
  * [The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/)
  * [TodoMVC](https://github.com/evancz/elm-todomvc)


### References

  * [Syntax](/docs/syntax)
  * [Style Guide](/docs/style-guide)
  * [Core Libraries](http://package.elm-lang.org/packages/elm-lang/core/latest/)
  * [Community Packages](http://package.elm-lang.org)
  * [Package Design](http://package.elm-lang.org/help/design-guidelines) /
    [Documentation](http://package.elm-lang.org/help/documentation-format)

### Slow Start

  * [The Book]()
  * [Advanced Topics](/docs/advanced-topics)

"""
