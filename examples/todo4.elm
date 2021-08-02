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
  { input : String
  , items : List Item
  , nextId : Int
  }


type alias Item =
  { id : Int
  , todo : String
  }


init : Model
init =
  { input = ""
  , items = []
  , nextId = 1
  }


type Msg
  = OnWriting String
  | OnAddItem
  | OnRemoveItem Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnWriting new ->
      { model | input = new }

    OnAddItem ->
      let newItem =
            { id = model.nextId, todo = model.input }
      in
      { model
      | items = newItem :: model.items
      , input = ""
      , nextId = model.nextId + 1
      }

    OnRemoveItem selected ->
      let check item =
            item.id /= selected
      in
      { model | items = List.filter check model.items }


view : Model -> Html Msg
view model =
  viewContainer
    [ viewHeader
    , viewInput model
    , viewItems model
    ]


viewContainer : List (Html msg) -> Html msg
viewContainer =
  main_
    [ style "font-family" "'Helvetica Neue', Helvetica, Arial, sans-serif"
    , style "font-weight" "100"
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



viewInput : Model -> Html Msg
viewInput model =
  div
    [ style "border" "1px solid lightgray"
    ]
    [ input
        [ type_ "text"
        , onInput OnWriting
        , value model.input
        , style "padding" "10px 10px"
        , style "width" "85%"
        , style "box-sizing" "border-box"
        , style "font-family" "inherit"
        , style "font-size" "18px"
        , style "font-weight" "100"
        , style "border" "0"
        ]
        []
    , viewButton
        [ style "font-size" "18px"
        , style "width" "15%"
        ]
        { onClick = OnAddItem, label = "add" }
    ]


viewItems : Model -> Html Msg
viewItems model =
  ul
    [ style "list-style" "none"
    , style "padding" "0"
    , style "margin" "0"
    , style "border" "1px solid lightgray"
    , style "border-top" "0"
    , style "border-bottom" "0"
    ]
    (List.map viewItem model.items)


viewItem : Item -> Html Msg
viewItem item =
  li
    [ style "padding" "10px 20px"
    , style "border-bottom" "1px solid lightgray"
    , style "font-size" "18px"
    ]
    [ viewButton
        [ style "float" "right" ]
        { onClick = OnRemoveItem item.id, label = "╳" }
    , text item.todo
    ]


viewButton : List (Attribute msg) -> { onClick : msg, label : String } -> Html msg
viewButton customStyles config =
  let styles =
        [ style "background" "none"
        , style "border" "0"
        , style "font-family" "inherit"
        , style "font-weight" "100"
        ]
        ++ customStyles
  in
  button (onClick config.onClick :: styles)
    [ text config.label ]