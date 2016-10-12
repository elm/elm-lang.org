import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main =
  beginnerProgram { model = "", view = view, update = update }


-- UPDATE

type Msg = NewPassword String

update (NewPassword password) oldPassword =
  password


-- VIEW

view : String -> Html Msg
view password =
  div []
    [ input [ myStyle, type_ "password", placeholder "Password", onInput NewPassword ] []
    , div [ myStyle ] [ text password ]
    ]

myStyle : Attribute msg
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
