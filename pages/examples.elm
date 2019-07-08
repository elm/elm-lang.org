import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - Examples"
    Skeleton.Examples
    [ div (Center.styles "600px")
        [ h1 [] [ text "Examples" ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            ]
            [ viewExamples "Web Apps" "https://guide.elm-lang.org"
                [ "Buttons"
                , "Text Fields"
                , "Forms"
                , "HTTP"
                , "JSON"
                , "Random"
                , "Time"
                ]
            , viewExamples "Charts" "https://package.elm-lang.org/packages/terezka/line-charts/latest/"
                [ "Line Chart"
                , "Upload Data"
                ]
            , viewExamples "Playground" "https://package.elm-lang.org/packages/evancz/elm-playground/latest"
                [ "Triangles"
                , "Pig"
                , "Moving Square"
                , "Turtle"
                , "Mario"
                , "Zelda"
                ]
            , viewExamples "Files" "https://package.elm-lang.org/packages/elm/file/latest"
                [ "Upload"
                , "Drag-and-Drop"
                , "Image Previews"
                ]
            , viewExamples "Random" "https://package.elm-lang.org/packages/elm/random/latest"
                [ "Dice"
                , "Cards"
                , "Art"
                ]
            , viewExamples "Testing" "https://package.elm-lang.org/packages/elm-explorations/test/latest/"
                [ "Unit Tests"
                , "Fuzz Tests"
                ]
            , viewExamples "SVG" "https://package.elm-lang.org/packages/elm/svg/latest"
                [ "Shapes"
                , "Clock"
                , "Tangrams"
                ]
            , viewExamples "WebGL" "https://package.elm-lang.org/packages/elm-explorations/webgl/latest/"
                [ "Triangle"
                , "Cube"
                , "Crate"
                , "Thwomp"
                , "First Person"
                ]
            , viewExamples "Parsing" "https://package.elm-lang.org/packages/elm/parser/latest"
                [ "Math"
                , "MIPS"
                , "JSON"
                ]
            ]
        ]
    ]



-- VIEW EXAMPLES


viewExamples : String -> String -> List String -> Html msg
viewExamples sectionTitle externalLink examples =
  div
    [ style "width" "200px"
    ]
    [ h2 [ style "margin-bottom" "0" ] [ text sectionTitle ]
    , ul
        [ style "list-style-type" "none"
        , style "padding-left" "16px"
        , style "margin-top" "8px"
        ]
        (List.map viewExample examples ++ [ viewMore externalLink ])
    ]


viewExample : String -> Html msg
viewExample example =
  let
    url = "/examples/" ++ String.replace " " "-" (String.toLower example)
  in
  li [] [ a [ href url ] [ text example ] ]


viewMore : String -> Html msg
viewMore url =
  li [] [ a [ href url ] [ text "..." ] ]
