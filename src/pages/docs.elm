import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import TopBar
import Outline


main =
  div []
    [ TopBar.topBar "docs"
    , Center.markdown "600px" directions
    , div [ Center.style "600px" ]
        [ h1 [id "complete-guide"] [text "Complete Guide"]
        , ul [class "guide content"] (List.map viewChapter Outline.outline)
        ]
    ]


directions = """

# Documentation

### Quick Start

  * [For JS users](/docs/from-javascript)
  * [Make an HTML app](https://github.com/evancz/start-app)
  * [The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/)

### References

  * [Syntax](/docs/syntax)
  * [Style Guide](/docs/style-guide)
  * [Core Libraries](http://package.elm-lang.org/packages/elm-lang/core/latest/)
  * [Community Packages](http://package.elm-lang.org)

"""


viewChapter : (String, List String) -> Html
viewChapter (title, sections) =
  li []
    [ a [href ("/guide/" ++ format title)] [text title]
    , ul [] (List.map (\name -> li [] [ text name ]) sections)
    ]


format str =
  String.toLower str
    |> String.map (\c -> if c == ' ' then '-' else c)