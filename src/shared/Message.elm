module Message exposing (report)

import Html exposing (..)
import Html.Attributes exposing (..)
import ColorScheme as C
import Markdown


report message =
  Markdown.toHtml [ reportStyle ] message


(=>) = (,)


reportStyle =
  style
    [ "width" => "300px"
    , "margin" => "100px auto 0"
    , "background" => "#F5F5F5"
    , "padding" => "0 30px 10px"
    , "border-top" => "4px solid #60B5CC"
    ]
