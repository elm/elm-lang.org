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
  Model True True True



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
    [ checkbox model.notifications ToggleNotifications "Email Notifications"
    , checkbox model.autoplay ToggleAutoplay "Video Autoplay"
    , checkbox model.location ToggleLocation "Use Location"
    ]


checkbox : isChecked -> msg -> String -> Html msg
checkbox isChecked msg name =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg, checked isChecked ] []
    , text name
    ]
