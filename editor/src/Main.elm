port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Deps
import Header
import Hint
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Http



-- PORTS


port submissions : (String -> msg) -> Sub msg
port cursorMoves : (Maybe String -> msg) -> Sub msg
port importEndLines : Int -> Cmd msg



-- MAIN


main : Program String Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { token : Maybe String
  , table : Hint.Table
  , imports : Header.Imports
  , dependencies : DepsInfo
  }


type DepsInfo
  = Loading
  | Failure
  | Success Deps.Info



-- INIT


init : String -> ( Model, Cmd Msg )
init source =
  case Header.parse source of
    Nothing ->
      ( Model Nothing Hint.defaultTable Header.defaultImports Loading
      , fetchDepsInfo
      )

    Just (imports, importEnd) ->
      ( Model Nothing Hint.defaultTable imports Loading
      , Cmd.batch
          [ fetchDepsInfo
          , importEndLines importEnd
          ]
      )


fetchDepsInfo : Cmd Msg
fetchDepsInfo =
  Http.get
    { url = "https://worker.elm-lang.org/compile/deps-info.json"
    , expect = Http.expectJson GotDepsInfo Deps.decoder
    }




-- UPDATE


type Msg
  = Submitted String
  | CursorMoved (Maybe String)
  | GotDepsInfo (Result Http.Error Deps.Info)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Submitted source ->
      case Header.parse source of
        Nothing ->
          ( model, Cmd.none )

        Just (imports, importEnd) ->
          case model.dependencies of
            Failure ->
              ( model, Cmd.none )

            Loading ->
              ( model, Cmd.none )

            Success info ->
              ( { model
                    | table = Hint.buildTable imports info
                    , imports = imports
                }
              , importEndLines importEnd
              )

    CursorMoved token ->
      ( { model | token = token }
      , Cmd.none
      )

    GotDepsInfo result ->
      case result of
        Err _ ->
          ( { model | dependencies = Failure }
          , Cmd.none
          )

        Ok info ->
          ( { model
                | table = Hint.buildTable model.imports info
                , dependencies = Success info
            }
          , Cmd.none
          )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ submissions Submitted
    , cursorMoves CursorMoved
    ]



-- VIEW


view : Model -> Html msg
view model =
  case model.token of
    Nothing ->
      viewExamplesLink

    Just token ->
      lazy2 viewHint token model.table



-- VIEW EXAMPLES LINK


viewExamplesLink : Html msg
viewExamplesLink =
  div [ class "hint" ]
    [ text "More Examples "
    , a [ href "/examples", target "_blank" ] [ text "Here" ]
    ]



-- VIEW HINT


viewHint : String -> Hint.Table -> Html msg
viewHint token table =
  case Hint.lookup token table of
    Just info ->
      case info of
        Hint.Ambiguous ->
          viewExamplesLink

        Hint.Specific hint ->
          div [ class "hint" ]
            [ text "Hint: "
            , a [ href hint.href, target "_blank" ] [ text hint.text ]
            ]

    Nothing ->
      viewExamplesLink
