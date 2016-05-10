module Blog exposing (blog, docs, evan, michael, Date)

import Dict
import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Center
import Skeleton


(=>) = (,)


blog : String -> String -> Author -> Date -> List (Html msg) -> Html msg
blog title subtitle author date body =
  Skeleton.skeleton "blog"
    [ div [ style [ "padding" => "4em 0 1em", "text-align" => "center" ] ]
        [ div [ style [ "font-size" => "4em" ] ] [text title]
        , div [ style [ "font-size" => "1.5em" ] ] [text subtitle]
        , div [ class "author" ]
            [ text "by "
            , a [href author.url] [text author.name]
            , text (" / " ++ dateToString date)
            ]
        ]
    , div [] body
    ]


docs : String -> List (Html msg) -> Html msg
docs title body =
  Skeleton.skeleton "docs"
    [ div [ style [ "padding" => "4em 0 1em", "text-align" => "center" ] ]
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


dateToString : Date -> String
dateToString date =
  case Dict.get date.month months of
    Nothing ->
      Debug.crash "invalid date"

    Just month ->
      toString date.day ++ " " ++ month ++ " " ++ toString date.year



months : Dict.Dict Int String
months =
  Dict.fromList
    [ 1 => "Jan"
    , 2 => "Feb"
    , 3 => "Mar"
    , 4 => "Apr"
    , 5 => "May"
    , 6 => "June"
    , 7 => "July"
    , 8 => "Aug"
    , 9 => "Sep"
    , 10 => "Oct"
    , 11 => "Nov"
    , 12 => "Dec"
    ]