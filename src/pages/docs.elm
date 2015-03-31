import Html exposing (..)
import Html.Attributes exposing (..)
import String

import TopBar
import Outline


main =
  div []
    [ TopBar.topBar "docs"
    , ul [] (List.map viewChapter Outline.outline)
    ]


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