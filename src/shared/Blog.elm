module Blog (blog) where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Center
import TopBar


(=>) = (,)


type alias Author =
    { name : String
    , url : String
    }


blog : String -> String -> Author -> List Html -> Html
blog title subtitle author body =
  div []
    [ TopBar.topBar "blog"
    , div [ style [ "padding" => "4em 0 1em", "text-align" => "center" ] ]
        [ div [ style [ "font-size" => "4em" ] ] [text title]
        , div [ style [ "font-size" => "1.5em" ] ] [text subtitle]
        , div [ class "author" ] [ text "by ", a [href author.url] [text author.name] ]
        ]
    , div [] body
    ]