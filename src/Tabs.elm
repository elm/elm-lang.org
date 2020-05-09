module Tabs exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List String -> Html msg
view labels =
  div
    [ style "width" "100%"
    , class "newtabs"
    ]
    [ Html.nav [] (List.map tab labels)
    , Html.main [ class "newtabs-main" ] [ Html.text "Content area" ]
    ]


tab : String -> Html msg
tab label =
  a [ class "newtabs-tab" ]
    [ text label ]
