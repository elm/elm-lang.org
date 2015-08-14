import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import Signal exposing (Address)
import StartApp.Simple as StartApp


main =
  StartApp.start { model = initialModel, view = view, update = update }


-- MODEL

type alias Model =
  { red : Bool
  , underline : Bool
  , bold : Bool
  }


initialModel =
  Model False False True


-- UPDATE

type Action
  = Red Bool
  | Underline Bool
  | Bold Bool


update action model =
  case action of
    Red bool ->
        { model | red <- bool }

    Underline bool ->
        { model | underline <- bool }

    Bold bool ->
        { model | bold <- bool }


-- VIEW

view address model =
  div [] <|
    span [toStyle model] [text "Hello, how are you?!"]
    :: br [] []
    :: checkbox address model.red Red "red"
    ++ checkbox address model.underline Underline "underline"
    ++ checkbox address model.bold Bold "bold"


toStyle model =
  style
    [ ("color", if model.red then "red" else "black")
    , ("text-decoration", if model.underline then "underline" else "none")
    , ("font-weight", if model.bold then "bold" else "normal")
    ]


checkbox : Address Action -> Bool -> (Bool -> Action) -> String -> List Html
checkbox address isChecked tag name =
  [ input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message address << tag)
      ]
      []
  , text name
  , br [] []
  ]
