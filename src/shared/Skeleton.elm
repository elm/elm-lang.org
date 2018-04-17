module Skeleton exposing (skeleton)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



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

