import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - Documentation"
    Skeleton.Docs
    [ Center.markdown "600px" content
    , div [ class "docs-buttons" ]
        [ a [ href "https://guide.elm-lang.org/" ] [ text "Official Guide" ]
        , a [ href "https://package.elm-lang.org/" ] [ text "Package Docs" ]
        ]
    , Center.markdown "600px" additionalResources
    ]


content = """

# Documentation

The best place to start is the official guide. It will give you a solid foundation for creating applications with Elm. Once you have worked through that, the next place to look for documentation is on the packages you are using.

"""


additionalResources = """

### Additional Resources

* [FAQ](http://elm-community.github.io/elm-faq/)
* [Syntax](/docs/syntax)
* [Syntax vs JS](/docs/from-javascript)
* [Style Guide](/docs/style-guide)
* [Package Design](http://package.elm-lang.org/help/design-guidelines)
* [Writing Documentation](http://package.elm-lang.org/help/documentation-format)
* [Advanced Topics](/docs/advanced-topics)

"""
