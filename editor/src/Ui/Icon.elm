module Ui.Icon exposing (..)


import FeatherIcons as I
import Html exposing (..)


view : Maybe String -> I.Icon -> Html msg
view color icon =
  icon
    |> I.withSize 14
    |> I.withClass ("icon " ++ Maybe.withDefault "" color)
    |> I.toHtml []