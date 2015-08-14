import Html exposing (Html, Attribute, div, input, span, text, toElement)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String


main =
  StartApp.start { model = empty, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


empty : Model
empty =
  Model "" "" ""


-- UPDATE

type Action
    = Name String
    | Password String
    | PasswordAgain String


update : Action -> Model -> Model
update action model =
  case action of
    Name name ->
      { model | name <- name }

    Password password ->
      { model | password <- password }

    PasswordAgain password ->
      { model | passwordAgain <- password }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  let
    validationMessage =
      if model.password == model.passwordAgain then
        span [style [("color", "green")]] [text "Passwords Match!"]
      else
        span [style [("color", "red")]] [text "Passwords do not match :("]
  in
    div []
      [ field "text" address Name "User Name" model.name
      , field "password" address Password "Password" model.password
      , field "password" address PasswordAgain "Re-enter Password" model.passwordAgain
      , div [fieldNameStyle "300px"] [validationMessage]
      ]


field : String -> Address Action -> (String -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
    [ div [fieldNameStyle "160px"] [text name]
    , input
        [ type' fieldType
        , placeholder name
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]


fieldNameStyle : String -> Attribute
fieldNameStyle px =
  style
    [ ("width", px)
    , ("padding", "10px")
    , ("text-align", "right")
    , ("display", "inline-block")
    ]
