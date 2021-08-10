port module Ui.Editor exposing (..)


{-| Control the code editor.

Relies on code-editor.js being present.

-}


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onMouseOver, onMouseLeave)
import Html.Lazy exposing (..)
import Http
import Json.Encode as E
import Json.Decode as D
import Elm.Error as Error exposing (Region)

import Data.Deps as Deps
import Data.Header as Header
import Data.Hint as Hint
import Data.Problem as Problem



-- PORTS


port submitSource : String -> Cmd msg
port gotErrors : (E.Value -> msg) -> Sub msg
port gotSuccess : (() -> msg) -> Sub msg



-- MODEL


type alias Model =
  { source : String
  , hint : Maybe String
  , hintTable : Hint.Table
  , imports : Header.Imports
  , importEnd : Int
  , dependencies : DepsInfo
  , selection : Maybe Region
  , status : Status
  }


type Status
  = Changed (Maybe (List Problem.Problem))
  | Compiling (Maybe (List Problem.Problem))
  | Compiled
  | Problems (List Problem.Problem)
  | Failed String


type DepsInfo
  = Loading
  | Failure
  | Success Deps.Info


getCurrentProblems : Model -> List Problem.Problem
getCurrentProblems model =
  case model.status of
    Changed (Just problems)   -> problems
    Changed Nothing           -> []
    Compiling (Just problems) -> problems
    Compiling Nothing         -> []
    Problems problems         -> problems
    Failed msg                -> []
    Compiled                  -> []



setSelection : Region -> Model -> Model
setSelection region model =
  { model | selection = Just region }



-- INIT


init : String -> ( Model, Cmd Msg )
init source =
  let defaults =
        { source = source
        , hint = Nothing
        , hintTable = Hint.defaultTable
        , imports = Header.defaultImports
        , importEnd = 0
        , dependencies = Loading
        , selection = Nothing
        , status = Compiled
        }
  in
  case Header.parse source of
    Nothing ->
      ( defaults
      , fetchDepsInfo
      )

    Just ( imports, importEnd ) ->
      ( { defaults | imports = imports, importEnd = importEnd }
      , fetchDepsInfo
      )


fetchDepsInfo : Cmd Msg
fetchDepsInfo =
  Http.get
    { url = "http://localhost:8000/compile/deps-info.json" -- TODO worker.elm-lang.org
    , expect = Http.expectJson GotDepsInfo Deps.decoder
    }



-- UPDATE


type Msg
  = OnChange String (Maybe Region)
  | OnSave String (Maybe Region)
  | OnHint (Maybe String)
  | OnCompile
  | GotDepsInfo (Result Http.Error Deps.Info)
  | GotSuccess
  | GotErrors E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnChange source selection ->
      ( { model
          | source = source
          , selection = selection
          , status =
              case model.status of
                Changed problems  -> Changed problems
                Problems problems -> Changed (Just problems)
                _               -> Changed Nothing
      }
      , Cmd.none
      )

    OnHint hint ->
      ( { model | hint = hint }, Cmd.none )

    OnSave source selection ->
      ( updateImports
          { model | source = source, selection = selection
          , status =
              case model.status of
                Changed problems  -> Compiling problems
                Problems problems -> Compiling (Just problems)
                _               -> Compiling Nothing
          }
      , submitSource source
      )

    OnCompile ->
      ( updateImports
          { model | status =
              case model.status of
                Changed problems  -> Compiling problems
                Problems problems -> Compiling (Just problems)
                _               -> Compiling Nothing
          }
      , submitSource model.source
      )

    GotDepsInfo result ->
      case result of
        Err _ ->
          ( { model | dependencies = Failure }
          , Cmd.none
          )

        Ok info ->
          ( { model | hintTable = Hint.buildTable model.imports info, dependencies = Success info }
          , Cmd.none
          )

    GotSuccess ->
      ( { model | status = Compiled }, Cmd.none )

    GotErrors value ->
      case D.decodeValue Error.decoder value of
        Ok errors ->
          ( { model | status = Problems (Problem.toIndexedProblems errors) }, Cmd.none )

        Err _ ->
          ( { model | status = Failed "Could not decode compilation problems." }, Cmd.none )


updateImports : Model -> Model
updateImports model =
  case Header.parse model.source of
    Nothing ->
      model

    Just ( imports, importEnd ) ->
      case model.dependencies of
        Failure ->
          model

        Loading ->
          model

        Success info ->
          { model
            | hintTable = Hint.buildTable imports info
            , imports = imports
            , importEnd = importEnd
          }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ gotErrors GotErrors, gotSuccess (always GotSuccess) ]



-- VIEW


viewEditor : Bool -> Model -> Html Msg
viewEditor isLight model =
  Html.form
    [ id "editor"
    , action "http://localhost:8000/compile/v2"
    , method "post"
    , enctype "multipart/form-data"
    , target "output"
    ]
    [ textarea [ id "code", name "code", style "display" "none" ] []
    , lazy4 viewEditor_ model.source model.selection isLight model.importEnd
    ]


viewEditor_ : String -> Maybe Region -> Bool -> Int -> Html Msg
viewEditor_ source selection lights importEnd =
  let theme =
        if lights then "light" else "dark"
  in
  node "code-editor"
    [ property "source" (E.string source)
    , property "theme" (E.string theme)
    , property "importEnd" (E.int importEnd)
    , property "selection" <|
        case selection of
          Nothing -> encodeBlankSelection
          Just region -> encodeSelection region
    , on "save" (D.map2 OnSave decodeSource decodeSelection)
    , on "change" (D.map2 OnChange decodeSource decodeSelection)
    , on "hint" (D.map OnHint decodeHint)
    ]
    []



-- VIEW / HINT


viewHint : Model -> Html msg
viewHint model =
  case model.hint of
    Nothing ->
      text ""

    Just hint ->
      lazy2 viewHint_ hint model.hintTable


viewHint_ : String -> Hint.Table -> Html msg
viewHint_ token table =
  case Hint.lookup token table of
    Just info ->
      case info of
        Hint.Ambiguous ->
          text ""

        Hint.Specific hint ->
          div
            [ class "hint" ]
            [ span [ style "margin-right" "20px" ] [ text "·" ]
            , text "Hint: "
            , a [ href hint.href, target "_blank" ] [ text hint.text ]
            ]

    Nothing ->
      text ""



-- ENCODE / DECODE


encodeSelection : Region -> E.Value
encodeSelection { start, end } =
  E.object
    [ ( "start", E.object [ ( "line", E.int start.line ), ( "column", E.int start.column ) ] )
    , ( "end", E.object [ ( "line", E.int end.line ), ( "column", E.int end.column ) ] )
    ]


encodeBlankSelection : E.Value
encodeBlankSelection =
  E.object
    [ ( "start", E.null )
    , ( "end", E.null )
    ]


decodeSource : D.Decoder String
decodeSource =
  D.at [ "target", "source" ] D.string


decodeSelection : D.Decoder (Maybe Region)
decodeSelection =
  D.at [ "target", "selection" ] <|
    D.map2 (Maybe.map2 Region)
      (D.field "start" (D.nullable decodePosition))
      (D.field "end" (D.nullable decodePosition))


decodePosition : D.Decoder Error.Position
decodePosition =
  D.map2 Error.Position
    (D.field "line" D.int)
    (D.field "column" D.int)


decodeHint : D.Decoder (Maybe String)
decodeHint =
  D.at [ "target", "hint" ] (D.nullable D.string)

