module Message exposing (report)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


report : String -> Html msg
report message =
  Markdown.toHtml
    [ style "width" "300px"
    , style "margin" "100px auto 0"
    , style "background" "#F5F5F5"
    , style "padding" "0 30px 10px"
    , style "border-top" "4px solid #60B5CC"
    ]
    message
