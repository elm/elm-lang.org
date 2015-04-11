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
--    , ul [ Center.style "600px" ] (List.map viewChapter Outline.outline)
    ]


directions = """

# Documentation


## Tutorials

  * [Reading Elm code](/)
  * [Building widgets](https://github.com/evancz/elm-architecture-tutorial/)
  * [Making Pong](/)


## About Elm

  * [Complete Guide to Elm](/)
  * [Syntax Reference](/learn/syntax)


## API References

  * [Standard Libraries](http://package.elm-lang.org/packages/elm-lang/core/latest/)
  * [Community Libraries](http://package.elm-lang.org/packages)

"""


viewChapter : (String, List String) -> Html
viewChapter (title, sections) =
  li []
    [ a [href ("/guide/" ++ format title)] [text title]
    , ul []
        (List.map (\name -> li [] [ text name ]) sections)
    ]

format str =
  String.toLower str
    |> String.map (\c -> if c == ' ' then '-' else c)