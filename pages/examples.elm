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
            [ viewExamples "HTML"
                [ "Hello"
                , "Groceries"
                , "Shapes"
                ]
            , viewExamples "User Input"
                [ "Buttons"
                , "Text Fields"
                , "Forms"
                ]
            , viewExamples "Random"
                [ "Numbers"
                , "Cards"
                , "Positions"
                ]
            , viewExamples "HTTP"
                [ "Book"
                , "Quotes"
                ]
            , viewExamples "Time"
                [ "Time"
                , "Clock"
                ]
            , viewExamples "Files"
                [ "Upload"
                , "Drag-and-Drop"
                , "Image Previews"
                ]
            , viewExamples "WebGL"
                [ "Triangle"
                , "Cube"
                , "Crate"
                , "Thwomp"
                , "First Person"
                ]
            , viewExamples "Playground"
                [ "Picture"
                , "Animation"
                , "Mouse"
                , "Keyboard"
                , "Turtle"
                , "Mario"
                ]
            ]
        , p [ style "margin-top" "3em" ]
            [ span [ style "font-weight" "bold" ] [ text "Reminder:" ]
            , text " Read through "
            , a [ href "https://guide.elm-lang.org" ] [ text "The Official Guide" ]
            , text " to learn the basics of Elm. It will help a lot with understanding these examples!"
            ]
        ]
    ]



-- VIEW EXAMPLES


viewExamples : String -> List String -> Html msg
viewExamples sectionTitle examples =
  div
    [ style "width" "200px"
    ]
    [ h2 [ style "margin-bottom" "0" ] [ text sectionTitle ]
    , ul
        [ style "list-style-type" "none"
        , style "padding-left" "16px"
        , style "margin-top" "8px"
        ]
        (List.map viewExample examples)
    ]


viewExample : String -> Html msg
viewExample example =
  let
    url = "/examples/" ++ String.replace " " "-" (String.toLower example)
  in
  li [] [ a [ href url ] [ text example ] ]
