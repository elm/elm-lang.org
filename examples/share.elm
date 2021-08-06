module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Int


init : Model
init =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "margin" "100px auto"
    , style "padding" "40px"
    , style "text-align" "center"
    , style "font-family" "\"IBM Plex Sans\", sans-serif"
    ]
    [ Html.node "link" [ rel "stylesheet", href "https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Courier+Prime&display=swap" ] []
    , h1
        [ style "font-size" "60px"
        , style "font-weight" "300"
        , style "margin-bottom" "10px"
        ]
        [ text "elm-share" ]
    , h2
        [ style "font-size" "20px"
        , style "font-weight" "300"
        , style "margin-top" "10px"
        ]
        [ text "Easily build and share your Elm website" ]
    , viewButton "Try it out"
    ]


viewButton : String -> Html Msg
viewButton text_ =
  button
    [ onClick Increment
    , style "margin" "0 auto"
    , style "border" "2px solid rgb(18, 147, 216)"
    , style "box-shadow" "5px 5px 0px 1px rgba(18,147,216,1)"
    , style "background" "white"
    , style "padding" "10px 20px"
    , style "font-size" "16px"
    , style "font-family" "\"IBM Plex Sans\", sans-serif"
    , style "width" "300px"
    ]
    [ text text_ ]