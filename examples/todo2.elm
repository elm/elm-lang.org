import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Html msg
main =
  viewContainer
    [ viewHeader
    , viewItems
    ]


viewContainer : List (Html msg) -> Html msg
viewContainer =
  main_
    [ style "font-family" "'Helvetica Neue', Helvetica, Arial, sans-serif"
    , style "font-weight" "300"
    , style "max-width" "400px"
    , style "margin" "0 auto"
    ]


viewHeader : Html msg
viewHeader =
  h1
    [ style "font-size" "100px"
    , style "font-weight" "100"
    , style "text-align" "center"
    , style "margin-bottom" "20px"
    ]
    [ text "todos" ]


viewItems : Html msg
viewItems =
  ul
    [ style "list-style" "none"
    , style "padding" "0"
    , style "border" "1px solid lightgray"
    , style "border-bottom" "0"
    ]
    (List.map viewItem items)


viewItem : Item -> Html msg
viewItem item =
  li
    [ style "padding" "10px 20px"
    , style "border-bottom" "1px solid lightgray"
    ]
    [ text item.todo ]


type alias Item =
  { todo : String }


items : List Item
items =
  [ { todo = "Get groceries" }
  , { todo = "Feed cat" }
  , { todo = "Answer letter from grandma" }
  ]