module Skeleton exposing
  ( Tab(..)
  , header
  , footer
  --
  , docs
  , hint
  , skeleton
  , news
  , Author
  , evan
  , michael
  , Date
  )


import Browser
import Center
import Grid
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Element as E
import Element.Font as F
import Element.Region as R



-- SKELETON


skeleton : String -> Tab -> List (Html Never) -> Program () () Never
skeleton title tab content =
  Browser.document
    { init = \_ -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \_ ->
        { title = title
        , body =
            [ Grid.view
            , E.layout [ E.width E.fill ] <|
                E.column []
                  [ header tab
                  , E.html (div [ style "flex" "1" ] content)
                  , E.html footer
                  ]
            ]
        }
    }



-- HEADER


type Tab
  = Examples
  | Docs
  | Community
  | News
  | Other


header : Tab -> E.Element msg
header tab =
  E.row
    [ E.width E.fill, E.centerX, R.navigation ]
    [ E.el [ E.alignLeft, E.alignBottom, F.size 30 ] (E.text "elm")
    , E.row [ E.alignRight, E.alignBottom, E.spacing 15, E.moveUp 3 ]
        [ E.el [ F.size 15 ] (E.text "overview")
        , E.el [ F.size 15 ] (E.text "featured")
        , E.el [ F.size 15 ] (E.text "examples")
        , E.el [ F.size 15 ] (E.text "documentation")
        , E.el [ F.size 15 ] (E.text "community")
        , E.el [ F.size 15 ] (E.text "news")
        , E.el [ F.size 15 ] (E.text "limitations")
        ]
    ]


viewTab : Tab -> Tab -> String -> String -> Html msg
viewTab currentTab targetTab name link =
  let
    attrs =
      if currentTab == targetTab then
        [ style "font-weight" "bold" ]
      else
        []
  in
  a (href link :: attrs) [ text name ]



-- FOOTER


footer : Html msg
footer =
  div [class "footer"]
    [ a [ class "grey-link", href "https://guide.elm-lang.org/install/elm.html" ] [ text "Install" ]
    , text " — "
    , a [ class "grey-link", href "https://github.com/elm/compiler/" ] [ text "Compiler Source" ]
    , text " — "
    , a [ class "grey-link", href "https://github.com/elm/elm-lang.org/" ] [ text "Site Source" ]
    , text " — © 2012-2020 Evan Czaplicki"
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



-- NEWS


news : String -> String -> Author -> Date -> List (Html Never) -> Program () () Never
news title subtitle author date body =
  skeleton title News
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
  , url = "https://twitter.com/evancz"
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
