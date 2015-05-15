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

  * [Reading Elm code](/)
  * [Building widgets](https://github.com/evancz/elm-architecture-tutorial/)
  * [Making Pong](/)

### References

  * [Syntax](/docs/syntax)
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