import Html exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)


main =
  beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
  { red : Bool
  , underline : Bool
  , bold : Bool
  }


model : Model
model =
  Model False False True



-- UPDATE


type Msg
  = Red Bool
  | Underline Bool
  | Bold Bool


update : Msg -> Model -> Model
update msg model =
  case msg of
    Red bool ->
        { model | red = bool }

    Underline bool ->
        { model | underline = bool }

    Bold bool ->
        { model | bold = bool }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ span [toStyle model] [text "Hello, how are you?!"]
    , label []
        [ br [] []
        , input [ type' "checkbox", checked model.red, onCheck Red ] []
        , text "red"
        ]
    , label []
        [ br [] []
        , input [ type' "checkbox", checked model.underline, onCheck Underline ] []
        , text "underline"
        ]
    , label []
        [ br [] []
        , input [ type' "checkbox", checked model.bold, onCheck Bold ] []
        , text "bold"
        ]
    ]


toStyle : Model -> Attribute msg
toStyle model =
  style
    [ ("color", if model.red then "red" else "black")
    , ("text-decoration", if model.underline then "underline" else "none")
    , ("font-weight", if model.bold then "bold" else "normal")
    ]


-- Exercise: move the repetative code in `view` into a separate function
-- and use it three times.
