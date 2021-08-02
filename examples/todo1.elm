import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Html msg
main =
  main_ []
    [ h1 [] [ text "todos" ]
    , ul [] (List.map viewItem items)
    ]


viewItem : Item -> Html msg
viewItem item =
  li [] [ text item.todo ]


type alias Item =
  { todo : String }


items : List Item
items =
  [ { todo = "Get groceries" }
  , { todo = "Feed cat" }
  , { todo = "Answer letter from grandma" }
  ]
