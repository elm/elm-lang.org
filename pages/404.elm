
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () () Never
main =
  Browser.document
    { init = \_ -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \_ ->
        { title = "Page not found"
        , body = [ notFound ]
        }
    }


notFound : Html msg
notFound =
  div
    [ style "width" "100vw"
    , style "height" "100vh"
    , style "background-color" "#eee"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]
    [ div
        [ style "max-width" "300px"
        , style "border-top" "4px solid #1293D8"
        , style "background-color" "white"
        , style "font-family" "'IBM Plex Sans',sans-serif"
        , style "padding" "0 2em 1em"
        , style "margin" "2em"
        ]
        [ h1 [ style "font-weight" "normal" ] [ text "Page Not Found" ]
        , p []
            [ text "I shall be telling this with a sigh", br [] []
            , text "Somewhere ages and ages hence:", br [] []
            , text "Two roads diverged in a wood, and I—", br [] []
            , text "I took the one less traveled by,", br [] []
            , text "And that has made all the difference."
            ]
        , p [ style "text-align" "right", style "font-style" "italic" ] [ text "Robert Frost" ]
        ]
    ]


