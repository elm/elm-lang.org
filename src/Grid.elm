module Grid exposing (view)

import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Svg msg
view =
    svg [ height "100%", width "100%", Svg.Attributes.style "position: absolute; height: 2300px; opacity: 0.5;" ]
        [ defs []
            [ node "pattern" [ height "5", id "smallGrid", patternUnits "userSpaceOnUse", width "5" ]
                [ Svg.path [ d "M 10 0 L 0 0 0 10", fill "none", stroke "gray", strokeWidth "1" ]
                    []
                , text "      "
                ]
            , node "pattern" [ height "10", id "medSmallGrid", patternUnits "userSpaceOnUse", width "10" ]
                [ node "rect" [ fill "url(#smallGrid)", height "100", width "100" ]
                    []
                , text "        "
                , Svg.path [ d "M 10 0 L 0 0 0 10", fill "none", stroke "black", strokeWidth "1" ]
                    []
                , text "      "
                ]
            , node "pattern" [ height "50", id "medLargeGrid", patternUnits "userSpaceOnUse", width "50" ]
                [ node "rect" [ fill "url(#medSmallGrid)", height "100", width "100" ]
                    []
                , text "        "
                , Svg.path [ d "M 10 0 L 0 0 0 10", fill "none", stroke "violet", strokeWidth "1" ]
                    []
                , text "      "
                ]
            , node "pattern" [ height "100", id "largeGrid", patternUnits "userSpaceOnUse", width "100" ]
                [ node "rect" [ fill "url(#medLargeGrid)", height "100", width "100" ]
                    []
                , text "        "
                , Svg.path [ d "M 100 0 L 0 0 0 100", fill "none", stroke "red", strokeWidth "1" ]
                    []
                , text "      "
                ]
            ]
        , node "rect" [ fill "url(#largeGrid)", height "100%", width "100%" ]
            []
        , text "  "
        ]