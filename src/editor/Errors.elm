foreign effect module Errors exposing (..)

import Html exposing (..)
import Html.App exposing (programWithFlags)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import String
import Task


foreign jumpTo : Region -> Cmd msg


main =
  programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }


boo = Task.succeed


-- MODEL

type alias Model =
  { sourceCode : String
  , errors : List Error
  }


init : Model -> (Model, Cmd msg)
init model =
  (model, Cmd.none)


-- UPDATE

type Msg
  = Jump Region

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Jump region ->
      (model, jumpTo region)


-- VIEW

view : Model -> Html Msg
view model =
  let
    lines =
      String.split "\n" model.sourceCode

    sortedErrors =
      List.sortBy (\e -> (getRegion e).start.line) model.errors
  in
    div [] (List.map (viewError lines) sortedErrors)


-- ERRORS

type alias Error =
  { region : Region
  , subregion : Maybe Region
  , tag : String
  , overview : String
  , details : String
  }


type alias Region =
  { start : Position
  , end : Position
  }


type alias Position =
  { line : Int
  , column : Int
  }


getRegion : Error -> Region
getRegion error =
  Maybe.withDefault error.region error.subregion


-- VIEW ERRORS

(=>) = (,)

viewError : List String -> Error -> Html Msg
viewError codeLines error =
  div
    [ style
        [ "white-space" => "pre"
        , "font-size" => "14px"
        ]
    ]
    [ div
        [ style
            [ "border-top" => "2px solid black"
            , "height" => "40px"
            ]
        ]
        [ h2
            [ style
                [ "margin" => "10px 0 0 0"
                , "display" => "inline-block"
                ]
            ]
            [ text error.tag ]
        , a
            [ onClick (Jump (getRegion error))
            , href "javascript:void(0)"
            , style ["float" => "right", "padding" => "1em"]
            ]
            [ text "jump to error" ]
        ]
    , pcode [] [text error.overview]
    , viewCode codeLines (getRegion error)
    , pcode [] [text error.details]
    , br [] []
    ]


pcode attrs html =
  p [] [code attrs html]


-- VIEW CODE

viewCode : List String -> Region -> Html msg
viewCode codeLines region =
  let
    start =
      region.start.line

    end =
      region.end.line

    subLines =
      codeLines
        |> List.drop (start - 2)
        |> List.take (end - start + 1)

    markdown =
      "```elm\n" ++ String.join "\n" subLines ++ "```"
  in
    Markdown.toHtml [] markdown

