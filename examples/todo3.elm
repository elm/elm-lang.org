
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }


type alias Model =
  { items : List Item }


type alias Item =
  { id : Int
  , todo : String
  }


init : Model
init =
  { items =
      [ { id = 1, todo = "Get groceries" }
      , { id = 2, todo = "Feed cat" }
      , { id = 3, todo = "Answer letter from grandma" }
      ]
  }


type Msg
  = OnRemoveItem Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnRemoveItem selected ->
      let check item =
            item.id /= selected
      in
      { model | items = List.filter check model.items }


view : Model -> Html Msg
view model =
  viewContainer
    [ viewHeader
    , viewItems model
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


viewItems : Model -> Html Msg
viewItems model =
  ul
    [ style "list-style" "none"
    , style "padding" "0"
    , style "border" "1px solid lightgray"
    , style "border-bottom" "0"
    ]
    (List.map viewItem model.items)


viewItem : Item -> Html Msg
viewItem item =
  li
    [ style "padding" "10px 20px"
    , style "border-bottom" "1px solid lightgray"
    ]
    [ viewButton item
    , text item.todo
    ]


viewButton : Item -> Html Msg
viewButton item =
  button
    [ style "background" "white"
    , style "border" "0"
    , style "float" "right"
    , onClick (OnRemoveItem item.id)
    ]
    [ text "╳" ]
