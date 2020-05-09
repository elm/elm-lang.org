module Tabs exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List (Bool, String) -> Html msg
view labels =
  div
    [ style "width" "100%"
    , class "newtabs"
    ]
    [ nav [] (List.map tab labels)
    , div [ class "newtabs-main" ] []
    ]


tab : (Bool, String )-> Html msg
tab (selected, label) =
  a [ class "newtabs-tab", if selected then class "selected" else class "" ]
    [ span [] [ text label ] ]
