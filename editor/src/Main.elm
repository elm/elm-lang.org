port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Deps
import Header
import Hint
import Navigation
import Errors
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
  , percentage : Percentage
  , selection : Maybe Error.Region
  , status : Status
  }


type Percentage
  = Moving Float Float
  | Percentage Float


type Status
  = Changed (Maybe Error.Error)
  | Compiling (Maybe Error.Error)
  | Compiled
  | Problems Error.Error
  | Failed String


type DepsInfo
  = Loading
  | Failure
  | Success Deps.Info



-- INIT


init : { original : String, name : String } -> ( Model, Cmd Msg )
init flags =
  let defaults =
        { token = Nothing
        , table = Hint.defaultTable
        , imports = Header.defaultImports
        , dependencies = Loading
        , source = flags.original
        , isLight = True
        , isMenuOpen = False
        , importEnd = 0
        , name = flags.name
        , percentage = Percentage 50
        , selection = Nothing
        , status = Compiled
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
  | OnDividerDown Float
  | OnDividerMove Float
  | OnDividerUp Float
  | OnLeftSideClick
  | OnJumpToProblem Error.Region
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
                Changed errors  -> Changed errors
                Problems errors -> Changed (Just errors)
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
                Changed errors  -> Compiling errors
                Problems errors -> Compiling (Just errors)
                _               -> Compiling Nothing
          }
      , submitSource source
      )

    OnCompile ->
      ( updateImports
          { model | status =
              case model.status of
                Changed errors  -> Compiling errors
                Problems errors -> Compiling (Just errors)
                _               -> Compiling Nothing
          }
      , submitSource model.source
      )

    GotSuccess ->
      ( { model | status = Compiled }, Cmd.none )

    GotErrors value ->
      case D.decodeValue Error.decoder value of
        Ok errors ->
          ( { model | status = Problems errors }, Cmd.none )

        Err _ ->
          ( { model | status = Failed "Could not decode errors." }, Cmd.none )

    OnDividerDown percentage ->
      ( { model | percentage = Moving percentage percentage }
      , Cmd.none
      )

    OnDividerMove latest ->
      let final =
            case model.percentage of
              Moving initial _ ->
                Moving initial latest

              Percentage previous ->
                Moving previous latest
      in
      ( { model | percentage = final }, Cmd.none )

    OnDividerUp latest ->
      let final =
            case model.percentage of
              Moving initial _ ->
                Percentage (if latest == initial then toNextPercentage latest else latest)

              Percentage _ ->
                Percentage latest
      in
      ( { model | percentage = final }
      , Cmd.none
      )

    OnLeftSideClick ->
      let final =
            case model.percentage of
              Moving _ latest ->
                Percentage (if latest <= 5 then 100 else latest)

              Percentage latest ->
                Percentage (if latest <= 5 then 100 else latest)
      in
      ( { model | percentage = final }, Cmd.none )

    OnJumpToProblem region ->
      ( { model | selection = Just region }, Cmd.none )

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


toNextPercentage : Float -> Float
toNextPercentage percentage =
  if percentage == 98 then 2.5 else
  if percentage == 2.5 then 98 else
  if percentage > 50 then 98 else
  if percentage <= 50 then 2.5
  else 2.5



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ gotErrors GotErrors, gotSuccess (always GotSuccess) ]



-- VIEW


view : Model -> Html Msg
view model =
  let hasErrors =
        case model.status of
          Changed (Just error) ->
            True

          Compiling (Just error) ->
            True

          Problems error ->
            True

          Failed msg ->
            True

          _ ->
            False

      preventEdges =
        Basics.max 2.5 >> Basics.min 98

      ( percentage, areColumnsMoving ) =
        case model.percentage of
          Moving _ latest ->
            ( preventEdges latest, True )

          Percentage latest ->
            ( preventEdges latest, False )
  in
  main_
    [ id "main"
    , classList
        [ ( "theme-light", model.isLight )
        , ( "theme-dark", not model.isLight )
        ]
    ]
    [ div
        [ id "double-pane"
        , style "width" "100%"
        , style "display" "flex"
        ]
        [ div
            [ id "left-side"
            , style "width" (String.fromFloat percentage ++ "%")
            , style "pointer-events" (if areColumnsMoving then "none" else "auto")
            , style "user-select" (if areColumnsMoving then "none" else "auto")
            , style "transition" (if areColumnsMoving then "none" else "width 0.5s")
            ]
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
            , div
                [ id "popup"
                , if percentage >= 98 then
                    style "transform" "translateY(0)"
                  else
                    style "transform" "translateY(100%)"
                , if percentage >= 98 then
                    style "transition-delay" "0.5s;"
                  else
                    style "transition-delay" "0s;"
                ]
                [ viewErrors model ]
            , viewNavigation model
            ]

        , node "column-divider"
            [ on "move" (D.map OnDividerMove (D.at [ "target", "percentage" ] D.float))
            , on "down" (D.map OnDividerDown (D.at [ "target", "percentage" ] D.float))
            , on "up" (D.map OnDividerUp (D.at [ "target", "percentage" ] D.float))
            , property "percentage" (E.float percentage)
            , if percentage == 100 then style "display" "none" else style "" ""
            ]
            []

        , div
            [ id "right-side"
            , style "width" (String.fromFloat (100 - percentage) ++ "%")
            , style "pointer-events" (if areColumnsMoving then "none" else "auto")
            , style "user-select" (if areColumnsMoving then "none" else "auto")
            , style "transition" (if areColumnsMoving then "none" else "width 0.5s")
            ]
            [ if percentage >= 98 then
                text ""
              else
                viewErrors model
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
    , onClick OnLeftSideClick
    ]
    []


viewErrors : Model -> Html Msg
viewErrors model =
  case model.status of
    Changed (Just error) ->
      Errors.view OnJumpToProblem error

    Compiling (Just error) ->
      Errors.view OnJumpToProblem error

    Problems error ->
      Errors.view OnJumpToProblem error

    Failed msg ->
      text msg -- TODO

    _ ->
      text ""



-- NAVIGATION


viewNavigation : Model -> Html Msg
viewNavigation model =
  Navigation.view
    { isLight = model.isLight
    , isOpen = model.isMenuOpen
    , left =
        [ Navigation.elmLogo
        , Navigation.lights OnToggleLights model.isLight
        , case model.token of
            Nothing ->
              text ""

            Just token ->
              lazy2 viewHint token model.table
        ]
    , right =
        [ Navigation.compilation OnCompile <|
            case model.status of
              Changed _   -> Navigation.Changed
              Compiling _ -> Navigation.Compiling
              Compiled    -> Navigation.Success
              Problems _  -> Navigation.ProblemsFound
              Failed _    -> Navigation.CouldNotCompile
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

