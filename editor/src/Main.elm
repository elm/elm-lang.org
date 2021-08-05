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
  , split : Float
  , isHoveringDivider : Bool
  , isMovingSplit : Bool
  , status : Status
  }


type Status
  = Changed (Maybe Error.Error)
  | Compiling
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
  case Header.parse flags.original of
    Nothing ->
      ( { token = Nothing
        , table = Hint.defaultTable
        , imports = Header.defaultImports
        , dependencies = Loading
        , source = flags.original
        , isLight = True
        , isMenuOpen = False
        , importEnd = 0
        , name = flags.name
        , split = 50
        , isHoveringDivider = False
        , isMovingSplit = False
        , status = Compiled
        }
      , fetchDepsInfo
      )

    Just ( imports, importEnd ) ->
      ( { token = Nothing
        , table = Hint.defaultTable
        , imports = imports
        , dependencies = Loading
        , source = flags.original
        , isLight = True
        , isMenuOpen = False
        , importEnd = importEnd
        , name = flags.name
        , split = 50
        , isHoveringDivider = False
        , isMovingSplit = False
        , status = Compiled
        }
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
  = OnChange String
  | OnSave String
  | Submitted String
  | GotDepsInfo (Result Http.Error Deps.Info)
  | OnCompile
  | OnToggleLights
  | OnToggleMenu
  | OnHint (Maybe String)
  | OnMoveSplit Float
  | GotErrors E.Value
  | OnDownSplit
  | OnUpSplit
  | GotSuccess ()
  | OnHoveringDivider Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnDownSplit ->
      ( { model | isMovingSplit = True }, Cmd.none )

    OnUpSplit ->
      ( { model | isMovingSplit = False }, Cmd.none )

    OnHoveringDivider isHovering ->
      ( { model | isHoveringDivider = isHovering }, Cmd.none )

    OnChange source ->
      ( { model
          | source = source
          , status =
              case model.status of
                Changed errors ->
                  Changed errors

                Problems errors ->
                  Changed (Just errors)

                _ ->
                  Changed Nothing
      }
      , Cmd.none
      )

    OnSave source ->
      ( { model | source = source, status = Compiling }
      , submitSource source
      )

    OnHint token ->
      ( { model | token = token }, Cmd.none )

    OnMoveSplit split ->
      ( { model | split = split }, Cmd.none )

    Submitted source ->
      case Header.parse source of
        Nothing ->
          ( model, Cmd.none )

        Just ( imports, importEnd ) ->
          case model.dependencies of
            Failure ->
              ( model, Cmd.none )

            Loading ->
              ( model, Cmd.none )

            Success info ->
              ( { model
                    | table = Hint.buildTable imports info
                    , imports = imports
                    , importEnd = importEnd
                }
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

    OnCompile ->
      ( { model | status = Compiling }, submitSource model.source )

    GotSuccess _ ->
      ( { model | status = Compiled }, Cmd.none )

    OnToggleLights ->
      ( { model | isLight = not model.isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

    GotErrors value ->
      case D.decodeValue Error.decoder value of
        Ok errors ->
          ( { model | status = Problems errors }, Cmd.none )

        Err _ ->
          ( { model | status = Failed "Could not decode errors." }, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ gotErrors GotErrors, gotSuccess GotSuccess ]



-- VIEW


view : Model -> Html Msg
view model =
  let split =
        if model.isHoveringDivider && not model.isMovingSplit then
          model.split |> Basics.max 3 |> Basics.min 97
        else if not model.isHoveringDivider && not model.isMovingSplit then
          if model.split < 3 then 0 else
          if model.split > 97 then 100
          else  model.split
        else
          model.split
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
            , style "width" (String.fromFloat split ++ "%")
            , style "pointer-events" (if model.isMovingSplit then "none" else "auto")
            , style "user-select" (if model.isMovingSplit then "none" else "auto")
            , style "transition" (if model.isMovingSplit then "none" else "width 0.1s")
            ]
            [ Html.form
                [ id "editor"
                , action "http://localhost:8000/compile/v2"
                , method "post"
                , enctype "multipart/form-data"
                , target "output"
                ]
                [ textarea [ id "code", name "code", style "display" "none" ] []
                , lazy3 viewEditor model.source model.isLight model.importEnd
                ]
            , viewNavigation model
            ]

        , node "split-page"
            [ on "move" (D.map OnMoveSplit (D.at [ "target", "split" ] D.float))
            , on "down" (D.succeed OnDownSplit)
            , on "up" (D.succeed OnUpSplit)
            , property "split" (E.float split)
            , onMouseOver (OnHoveringDivider True)
            , onMouseLeave (OnHoveringDivider False)
            ]
            []

        , div
            [ id "right-side"
            , style "width" (String.fromFloat (100 - split) ++ "%")
            , style "pointer-events" (if model.isMovingSplit then "none" else "auto")
            , style "user-select" (if model.isMovingSplit then "none" else "auto")
            , style "transition" (if model.isMovingSplit then "none" else "width 0.1s")
            ]
            [ case model.status of
                Changed (Just error) ->
                  Errors.view error

                Problems error ->
                  Errors.view error

                Failed msg ->
                  text msg -- TODO

                _ ->
                  text ""

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


viewEditor : String -> Bool -> Int -> Html Msg
viewEditor source lights importEnd =
  let theme =
        if lights then "light" else "dark"
  in
  node "code-editor"
    [ property "source" (E.string source)
    , property "theme" (E.string theme)
    , property "importEnd" (E.int importEnd)
    , on "save" (D.map OnSave (D.at [ "target", "source" ] D.string))
    , on "change" (D.map OnChange (D.at [ "target", "source" ] D.string))
    , on "hint" (D.map OnHint (D.at [ "target", "hint" ] (D.nullable D.string)))
    ]
    []



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
              Changed _  -> Navigation.Changed
              Compiling  -> Navigation.Compiling
              Compiled   -> Navigation.Success
              Problems _ -> Navigation.ProblemsFound
              Failed _   -> Navigation.CouldNotCompile
        --, Navigation.share OnToggleLights -- TODO
        , Navigation.deploy OnToggleLights -- TODO
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

