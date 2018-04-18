module Skeleton exposing
  ( docs
  , hints
  , skeleton
  , blog
  , Author
  , evan
  , michael
  , Date
  )


import Browser
import Center
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)



-- DOCS


docs : String -> List (Html Never) -> Program () () Never
docs title body =
  skeleton title "docs"
    [ div
        [ style "padding" "4em 0 1em"
        , style "text-align" "center"
        ]
        [ div [ style "font-size" "4em" ] [text title]
        ]
    , div [] body
    ]



-- HINTS


hints : String -> String -> Program () () Never
hints title markdown =
  docs title
    [ div
        [ style "max-width" "600px"
        , style "margin" "0 auto"
        ]
        [ Center.markdown "600px" markdown ]
    ]



-- SKELETON


skeleton : String -> String -> List (Html Never) -> Program () () Never
skeleton title tabName content =
  Browser.fullscreen
    { init = \_ -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    , onNavigation = Nothing
    , view = \_ ->
        { title = title
        , body = header tabName :: content ++ [footer]
        }
    }



-- HEADER


header : String -> Html msg
header name =
  div [ id "tabs" ]
    [ a [ href "/"
        , style "position" "absolute"
        , style "left" "1em"
        , style "top" "1em"
        ]
        [ img [ src "/assets/logo.svg", style "width" "24px" ] []
        ]
    , ul [] (List.map (tab name) [ "examples", "docs", "community", "blog" ])
    ]


tab : String -> String -> Html msg
tab currentName name =
  li []
    [ a [ classList [ ("tab", True), ("current", currentName == name) ]
        , href ("/" ++ name)
        ]
        [ text name ]
    ]



-- FOOTER


footer : Html msg
footer =
  div [class "footer"]
    [ text "All code for this site is open source and written in Elm. "
    , a [ class "grey-link", href "https://github.com/elm-lang/elm-lang.org/" ] [ text "Check it out" ]
    , text "! — © 2012-2018 Evan Czaplicki"
    ]



-- BLOG


blog : String -> String -> Author -> Date -> List (Html Never) -> Program () () Never
blog title subtitle author date body =
  skeleton title "blog"
    [ div
        [ style "padding" "4em 0 1em"
        , style "text-align" "center"
        ]
        [ div [ style "font-size" "4em" ] [text title]
        , div [ style "font-size" "1.5em" ] [text subtitle]
        , div [ class "author" ]
            [ text "by "
            , a [href author.url] [text author.name]
            , text (" / " ++ dateToString date)
            ]
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
      String.fromInt date.year

    Just month ->
      String.fromInt date.day ++ " " ++ month ++ " " ++ String.fromInt date.year



months : Dict.Dict Int String
months =
  Dict.fromList
    [ (1, "Jan")
    , (2, "Feb")
    , (3, "Mar")
    , (4, "Apr")
    , (5, "May")
    , (6, "June")
    , (7, "July")
    , (8, "Aug")
    , (9, "Sep")
    , (10, "Oct")
    , (11, "Nov")
    , (12, "Dec")
    ]
