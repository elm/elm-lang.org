module Skeleton exposing
  ( Tab(..)
  , header
  , footer
  --
  , docs
  , hint
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



-- SKELETON


skeleton : String -> Tab -> List (Html Never) -> Program () () Never
skeleton title tab content =
  Browser.document
    { init = \_ -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \_ ->
        { title = title
        , body = header tab :: content ++ [footer]
        }
    }



-- HEADER


type Tab
  = Examples
  | Docs
  | Community
  | Blog
  | Other


header : Tab -> Html msg
header tab =
  div [ class "header" ]
    [ div [ class "nav" ]
        [ a [ href "/"
            , style "color" "white"
            , style "font-size" "32px"
            ]
            [ text "elm"
            ]
        , div [ class "tabs" ] <| List.map (viewTab tab) <|
            [ TabInfo Examples "examples" "/examples"
            , TabInfo Docs "docs" "/docs"
            , TabInfo Community "community" "/community"
            , TabInfo Blog "blog" "/blog"
            ]
        ]
    ]


viewTab : Tab -> TabInfo -> Html msg
viewTab currentTab info =
  let
    attrs =
      if currentTab == info.tab then
        [ style "font-weight" "bold" ]
      else
        []
  in
  a (href info.link :: attrs) [ text info.name ]


type alias TabInfo =
  { tab : Tab
  , name : String
  , link : String
  }



-- FOOTER


footer : Html msg
footer =
  div [class "footer"]
    [ text "All code for this site is open source and written in Elm. "
    , a [ class "grey-link", href "https://github.com/elm/elm-lang.org/" ] [ text "Check it out" ]
    , text "! — © 2012-2019 Evan Czaplicki"
    ]



-- DOCS


docs : String -> List (Html Never) -> Program () () Never
docs title body =
  skeleton title Docs
    [ div
        [ style "padding" "4em 0 1em"
        , style "text-align" "center"
        ]
        [ div [ style "font-size" "4em" ] [text title]
        ]
    , div [] body
    ]



-- HINT


hint : String -> String -> Program () () Never
hint title markdown =
  docs title
    [ div
        [ style "max-width" "600px"
        , style "margin" "0 auto"
        ]
        [ Center.markdown "600px" markdown ]
    ]



-- BLOG


blog : String -> String -> Author -> Date -> List (Html Never) -> Program () () Never
blog title subtitle author date body =
  skeleton title Blog
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
