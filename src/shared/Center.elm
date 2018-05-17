module Center exposing
  ( markdown
  , styles
  )


import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Markdown



markdown : String -> String -> Html msg
markdown width string =
  div (class "content" :: styles width) [ Markdown.toHtml options [] string ]


styles : String -> List (Attribute msg)
styles width =
  [ style "display" "block"
  , style "max-width" width
  , style "margin" "0 auto"
  ]


options : Markdown.Options
options =
  { githubFlavored = Just { tables = False, breaks = False }
  , defaultHighlighting = Nothing
  , sanitize = False
  , smartypants = False
  }
