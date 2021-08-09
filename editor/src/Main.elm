port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onMouseOver, onMouseLeave)
import Html.Lazy exposing (..)
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (xlinkHref)
import Http
import Json.Encode as E
import Json.Decode as D
import Elm.Error as Error

import Data.Deps as Deps
import Data.Header as Header
import Data.Hint as Hint
import Data.Problem as Problem
import Data.Window exposing (Window)
import Ui.Problem
import Ui.Navigation
import Ui.ColumnDivider


-- TODO
-- Clean up styles
-- Fix column divider bugs
-- Make sure mini errors goes away when pane is moved in
-- Make state more exact
-- Make deps hints work again
-- See if hints can be optimized futher



-- PORTS


port submitSource : String -> Cmd msg
port gotErrors : (E.Value -> msg) -> Sub msg
port gotSuccess : (() -> msg) -> Sub msg



-- MAIN


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
  , source : String
  , isLight : Bool
  , isMenuOpen : Bool
  , importEnd : Int
  , name : String
  , divider : Ui.ColumnDivider.Model
  , selection : Maybe Error.Region
  , areProblemsMini : Bool
  , currentProblem : Int
  , status : Status
  , window : Window
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


getCurrentProblems : Status -> List Problem.Problem
getCurrentProblems status =
  case status of
    Changed (Just problems)   -> problems
    Changed Nothing           -> []
    Compiling (Just problems) -> problems
    Compiling Nothing         -> []
    Problems problems         -> problems
    Failed msg                -> []
    Compiled                  -> []



-- INIT


init : { original : String, name : String, width : Float, height : Float } -> ( Model, Cmd Msg )
init flags =
  let window =
        { width = flags.width, height = flags.height }

      defaults =
        { token = Nothing
        , table = Hint.defaultTable
        , imports = Header.defaultImports
        , dependencies = Loading
        , source = flags.original
        , isLight = True
        , isMenuOpen = False
        , importEnd = 0
        , name = flags.name
        , divider = Ui.ColumnDivider.init window
        , selection = Nothing
        , currentProblem = 0
        , areProblemsMini = False
        , status = Compiled
        , window = window
        }
  in
  case Header.parse flags.original of
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
    { url = "https://worker.elm-lang.org/compile/deps-info.json"
    , expect = Http.expectJson GotDepsInfo Deps.decoder
    }




-- UPDATE


type Msg
  = GotDepsInfo (Result Http.Error Deps.Info)
  | OnSourceChange String
  | OnSourceSave String
  | OnSourceHint (Maybe String)
  | OnCompile
  | GotSuccess
  | GotErrors E.Value
  | OnDividerMsg Ui.ColumnDivider.Msg
  | OnProblem Int
  | OnJumpToProblem Error.Region
  | OnMinimizeProblem Bool
  | OnToggleLights
  | OnToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotDepsInfo result ->
      case result of
        Err _ ->
          ( { model | dependencies = Failure }
          , Cmd.none
          )

        Ok info ->
          ( { model | table = Hint.buildTable model.imports info, dependencies = Success info }
          , Cmd.none
          )

    OnSourceChange source ->
      ( { model
          | source = source
          , status =
              case model.status of
                Changed problems  -> Changed problems
                Problems problems -> Changed (Just problems)
                _               -> Changed Nothing
      }
      , Cmd.none
      )

    OnSourceHint token ->
      ( { model | token = token }, Cmd.none )

    OnSourceSave source ->
      ( updateImports
          { model | source = source
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

    GotSuccess ->
      ( { model | status = Compiled }, Cmd.none )

    GotErrors value ->
      case D.decodeValue Error.decoder value of
        Ok errors ->
          ( { model | status = Problems (Problem.toIndexedProblems errors), currentProblem = 0 }, Cmd.none )

        Err _ ->
          ( { model | status = Failed "Could not decode compilation problems." }, Cmd.none )

    OnDividerMsg subMsg ->
      ( { model | divider = Ui.ColumnDivider.update model.window subMsg model.divider }
      , Cmd.none
      )

    OnProblem index ->
      ( { model | currentProblem = index }, Cmd.none )

    OnJumpToProblem region ->
      ( { model | selection = Just region }, Cmd.none )

    OnMinimizeProblem isMini ->
      ( { model | areProblemsMini = isMini }, Cmd.none )

    OnToggleLights ->
      ( { model | isLight = not model.isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )



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
            | table = Hint.buildTable imports info
            , imports = imports
            , importEnd = importEnd
          }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ gotErrors GotErrors, gotSuccess (always GotSuccess) ]



-- VIEW


view : Model -> Html Msg
view model =
  let currentProblems =
        getCurrentProblems model.status

      hasErrors =
        not (List.isEmpty currentProblems)
  in
  main_
    [ id "main"
    , classList
        [ ( "theme-light", model.isLight )
        , ( "theme-dark", not model.isLight )
        ]
    ]
    [ Ui.ColumnDivider.view OnDividerMsg model.window model.divider
        [ Html.form
            [ id "editor"
            , action "http://localhost:8000/compile/v2"
            , method "post"
            , enctype "multipart/form-data"
            , target "output"
            ]
            [ textarea [ id "code", name "code", style "display" "none" ] []
            , lazy4 viewEditor model.source model.selection model.isLight model.importEnd
            ]
        , if hasErrors && not model.areProblemsMini then
            div
              [ id "popup"
              , if Ui.ColumnDivider.isTooLarge model.window model.divider
                then style "transform" "translateY(0)"
                else style "transform" "translateY(100%)"
              , if Ui.ColumnDivider.isTooLarge model.window model.divider
                then style "transition-delay" "0.5s;"
                else style "transition-delay" "0s;"
              ]
              [ Ui.Problem.viewCurrent
                  { onJump = OnJumpToProblem
                  , onProblem = OnProblem
                  , onMinimize = OnMinimizeProblem True
                  , current = model.currentProblem
                  }
                  Ui.Problem.viewCarousel
                  currentProblems
              ]
          else
            text ""
        , viewNavigation model hasErrors currentProblems
        ]
        [ if not hasErrors || Ui.ColumnDivider.isTooLarge model.window model.divider then
            text ""
          else
            Ui.Problem.viewList OnJumpToProblem (getCurrentProblems model.status)
        , iframe
            [ id "output"
            , name "output"
            , case model.status of
                Changed (Just _) -> style "display" "none"
                Problems _ -> style "display" "none"
                _ -> style "display" "block"
            , src ("/examples/_compiled/" ++ model.name ++ ".html")
            ]
            []
        ]
    ]


viewEditor : String -> Maybe Error.Region -> Bool -> Int -> Html Msg
viewEditor source selection lights importEnd =
  let theme =
        if lights then "light" else "dark"
  in
  node "code-editor"
    [ property "source" (E.string source)
    , property "theme" (E.string theme)
    , property "importEnd" (E.int importEnd)
    , property "selection" <|
        case selection of
          Nothing ->
            E.object
              [ ( "start", E.null )
              , ( "end", E.null )
              ]

          Just { start, end } ->
            E.object
              [ ( "start", E.object [ ( "line", E.int start.line ), ( "column", E.int start.column ) ] )
              , ( "end", E.object [ ( "line", E.int end.line ), ( "column", E.int end.column ) ] )
              ]
    , on "save" (D.map OnSourceSave (D.at [ "target", "source" ] D.string))
    , on "change" (D.map OnSourceChange (D.at [ "target", "source" ] D.string))
    , on "hint" (D.map OnSourceHint (D.at [ "target", "hint" ] (D.nullable D.string)))
    --, onClick OnLeftSideClick TODO
    ]
    []



-- NAVIGATION


viewNavigation : Model -> Bool -> List Problem.Problem -> Html Msg
viewNavigation model hasErrors currentProblems =
  Ui.Navigation.view
    { isLight = model.isLight
    , isOpen = model.isMenuOpen
    , left =
        [ Ui.Navigation.elmLogo
        , Ui.Navigation.lights OnToggleLights model.isLight
        , case model.token of
            Nothing ->
              text ""

            Just token ->
              lazy2 viewHint token model.table
        ]
    , right =
        let problemEls =
              if hasErrors && model.areProblemsMini then
                [ Ui.Problem.viewCurrent
                    { onJump = OnJumpToProblem
                    , onProblem = OnProblem
                    , onMinimize = OnMinimizeProblem False
                    , current = model.currentProblem
                    }
                    Ui.Problem.viewCarouselMini
                    currentProblems
                , span [ style "margin-left" "10px" ] [ text "" ]
                ]
              else
                []

        in
        problemEls ++
        [ Ui.Navigation.compilation OnCompile <|
            case model.status of
              Changed _   -> Ui.Navigation.Changed
              Compiling _ -> Ui.Navigation.Compiling
              Compiled    -> Ui.Navigation.Success
              Problems _  -> Ui.Navigation.ProblemsFound
              Failed _    -> Ui.Navigation.CouldNotCompile
        ]
    }



-- VIEW HINT


viewHint : String -> Hint.Table -> Html msg
viewHint token table =
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


-- VIEW EXAMPLES LINK


viewExamplesLink : Html msg
viewExamplesLink =
  div [ class "hint" ]
    [ a [ href "/examples", target "_blank" ] [ text "More examples" ]
    ]

