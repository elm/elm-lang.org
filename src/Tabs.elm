module Tabs exposing (view)

import Html
import Html.Attributes
import Svg
import Svg.Attributes exposing (..)


view : List String -> Html.Html msg
view labels =
  Html.div
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "display" "flex"
    ]
    [ tabListStart
    , Html.div
        [ Html.Attributes.style "display" "flex" ]
        (List.map tab labels)
    , tabListEnd
    ]


tab : String -> Svg.Svg msg
tab label =
  Html.div
    [ Html.Attributes.style "display" "flex" ]
    [ tabStart
    , Html.div
      [ Html.Attributes.style "display" "inline-block"
      , Html.Attributes.style "font-size" "14px"
      , Html.Attributes.style "border-top" "2px solid #1293D8"
      , Html.Attributes.style "line-height" "40px"
      , Html.Attributes.style "letter-spacing" "2px"
      , Html.Attributes.style "font-weight" "bold"
      , Html.Attributes.style "height" "40px"
      ]
      [ Html.text (String.toUpper label) ]
    , tabEnd
    ]


tabStart : Svg.Svg msg
tabStart =
  Svg.svg
    [ viewBox "0 0 20 20", width "40", height "40", style "display: inline-block;" ]
    [ Svg.path
        [ d "m 0 18 h 5 a 7 7 0 0 0 6 -4 l 5 -11 a -3 -3 0 0 1 3 -2"
        , fill "transparent"
        , stroke "#1293D8"
        , strokeWidth "1"
        ]
        []
    ]


tabEnd : Svg.Svg msg
tabEnd =
  Svg.svg
    [ viewBox "0 0 20 20", width "40", height "40", style "display: inline-block;" ]
    [ Svg.path
        [ d "m 0 1 a 3 3 0 0 1 3 2 l 5 11 a 7 7 0 0 0 6 4 h 5"
        , fill "transparent"
        , stroke "#1293D8"
        , strokeWidth "1"
        ]
        []
    ]


tabListStart : Svg.Svg msg
tabListStart =
  Svg.svg
    [ viewBox "0 0 26 30", width "52", height "60", style "display: inline-block;" ]
    [ Svg.path
        [ d "m 0 29 a 7 7 0 0 1 6 -4 h 20"
        , fill "transparent"
        , stroke "#1293D8"
        , strokeWidth "1"
        ]
        []
    ]


tabListEnd : Svg.Svg msg
tabListEnd =
  Svg.svg
    [ viewBox "0 0 26 30", width "52", height "60", style "display: inline-block;" ]
    [ Svg.path
        [ d "m 0 25 h 20 a 7 7 0 0 1 6 4"
        , fill "transparent"
        , stroke "#1293D8"
        , strokeWidth "1"
        ]
        []
    ]
