-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)


main =
  beginnerProgram { model = model, view = view, update = update }
  

-- MODEL

model = 0


-- UPDATE

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
      

-- VIEW

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
