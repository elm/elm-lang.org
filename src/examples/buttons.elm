import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp


{-| Read more about StartApp and how this works at:

    https://github.com/evancz/start-app

The rough idea is that we just specify a model, a way to view it,
and a way to update it. That's all there is to it!
-}
main =
  StartApp.start { model = 0, view = view, update = update }


view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


type Action = Increment | Decrement


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
