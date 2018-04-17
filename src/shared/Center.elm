module Center exposing
  ( markdown
  , styles
  )


import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Markdown



markdown : String -> String -> Html msg
markdown width string =
  div (class "content" :: styles width) [ Markdown.toHtml [] string ]


styles : String -> List (Attribute msg)
styles width =
  [ style "display" "block"
  , style "max-width" width
  , style "margin" "0 auto"
  ]
