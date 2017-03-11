module Main exposing (..)

import Html exposing (Html, beginnerProgram, fieldset, input, label, text)
import Html.Attributes exposing (style, type_, checked)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = optOut, update = update, view = view }



-- MODEL


type alias Model =
    { notifications : Bool
    , autoplay : Bool
    , location : Bool
    }


optOut : Model
optOut =
    Model False False False



-- UPDATE


type Msg
    = ToggleNotifications
    | ToggleAutoplay
    | ToggleLocation


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleNotifications ->
            { model | notifications = not model.notifications }

        ToggleAutoplay ->
            { model | autoplay = not model.autoplay }

        ToggleLocation ->
            { model | location = not model.location }



-- VIEW


view : Model -> Html Msg
view model =
    fieldset []
        [ checkbox ToggleNotifications "Email Notifications" model.notifications
        , checkbox ToggleAutoplay "Video Autoplay" model.autoplay
        , checkbox ToggleLocation "Use Location" model.location
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isChecked =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , onClick msg
            ]
            []
        , text name
        ]
