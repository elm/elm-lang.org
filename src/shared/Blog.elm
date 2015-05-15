module Blog (blog, docs, evan, michael, Date) where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Center
import TopBar


(=>) = (,)


blog : String -> String -> Author -> Date -> List Html -> Html
blog title subtitle author date body =
  div []
    [ TopBar.topBar "blog"
    , div [ style [ "padding" => "4em 0 1em", "text-align" => "center" ] ]
        [ div [ style [ "font-size" => "4em" ] ] [text title]
        , div [ style [ "font-size" => "1.5em" ] ] [text subtitle]
        , div [ class "author" ] [ text "by ", a [href author.url] [text author.name] ]
        ]
    , div [] body
    ]


docs : String -> List Html -> Html
docs title body =
  div []
    [ TopBar.topBar "docs"
    , div [ style [ "padding" => "4em 0 1em", "text-align" => "center" ] ]
        [ div [ style [ "font-size" => "4em" ] ] [text title]
        ]
    , div [] body
    ]


-- AUTHORS

type alias Author =
    { name : String
    , url : String
    }


evan : Author
evan =
    { name = "Evan Czaplicki"
    , url = "https://twitter.com/czaplic"
    }


michael : Author
michael =
    { name = "Michael James"
    , url = "http://github.com/michaelbjames"
    }


-- DATES

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


