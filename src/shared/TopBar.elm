module TopBar (topBar) where

import Html exposing (..)
import Html.Attributes exposing (..)


(=>) = (,)


topBar name =
  div [ id "tabs" ]
    [ a [ href "/", style [ "position" => "absolute", "left" => "1em", "top" => "1em" ] ]
        [ img [ src "/assets/logo.svg", style [ "width" => "24px" ] ] []
        ]
    , ul [] (List.map (tab name) [ "examples", "docs", "community", "blog" ])
    ]


tab currentName name =
  li []
    [ a [ classList [ "tab" => True, "current" => (currentName == name) ]
        , href ("/" ++ name)
        ]
        [ text name ]
    ]

