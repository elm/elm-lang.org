port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Deps
import Header
import Hint
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
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
  , isMovingSplit : Bool
  , errors : Maybe (Result D.Error Error.Error)
  }


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
        , isMovingSplit = False
        , errors = Nothing
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
        , isMovingSplit = False
        , errors = Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnDownSplit ->
      ( { model | isMovingSplit = True }, Cmd.none )

    OnUpSplit ->
      ( { model | isMovingSplit = False }, Cmd.none )

    OnChange source ->
      ( { model | source = source }, Cmd.none )

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
      ( { model | errors = Nothing }, submitSource model.source )

    OnToggleLights ->
      ( { model | isLight = not model.isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

    GotErrors value ->
      ( { model | errors = Just <| D.decodeValue Error.decoder value }, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  gotErrors GotErrors



-- VIEW


view : Model -> Html Msg
view model =
  main_
    [ id "main" ]
    [ viewNavigation model

    , div
        [ style "width" "100%"
        , style "display" "flex"
        ]
        [ Html.form
            [ id "editor"
            , action "http://localhost:8000/compile/v2"
            , method "post"
            , enctype "multipart/form-data"
            , target "output"
            , style "width" (String.fromFloat model.split ++ "%")
            , style "pointer-events" (if model.isMovingSplit then "none" else "auto")
            ]
            [ textarea [ id "code", name "code", style "display" "none" ] []
            , lazy3 viewEditor model.source model.isLight model.importEnd
            ]

        , node "split-page"
            [ on "move" (D.map OnMoveSplit (D.at [ "target", "split" ] D.float))
            , on "down" (D.succeed OnDownSplit)
            , on "up" (D.succeed OnUpSplit)
            , property "split" (E.float model.split)
            ]
            []

        , div
            [ style "width" (String.fromFloat (100 - model.split) ++ "%")
            , style "pointer-events" (if model.isMovingSplit then "none" else "auto")
            ]
            [ viewErrors model
            , iframe
              [ id "output"
              , name "output"
              , case model.errors of
                  Just (Ok _) -> style "display" "none"
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
    , on "change" (D.map OnChange (D.at [ "target", "source" ] D.string))
    , on "hint" (D.map OnHint (D.at [ "target", "hint" ] (D.nullable D.string)))
    ]
    []


viewErrors : Model -> Html Msg
viewErrors model =
  case model.errors of
    Nothing ->
      text ""

    Just (Err _) ->
      text "Could not decode errors" -- TODO

    Just (Ok (Error.GeneralProblem problem)) ->
      div
        [ id "errors" ]
        [ viewError problem.title problem.message (Err problem.path) ]

    Just (Ok (Error.ModuleProblems modules)) ->
      let viewModuleError module_ =
            div [ id "error-module" ] (List.map viewProblem module_.problems)

          viewProblem problem =
            viewError problem.title problem.message (Ok problem.region)
      in
      div
        [ id "errors" ]
        (List.map viewModuleError modules)


viewError : String -> List Error.Chunk -> Result (Maybe String) Error.Region -> Html Msg
viewError title message area =
  div [ class "error-container" ]
    [ div
        [ class "error-header" ]
        [ div [ class "error-title"] [ text title ]
        , case area of
            Ok region -> div [ class "error-region"] [ text "Jump to problem" ]
            Err (Just name) -> div [ class "error-region"] [ text name ]
            Err Nothing -> text ""
        ]
    , div [ class "error-body" ] (viewMessage message)
    ]


viewMessage : List Error.Chunk -> List (Html msg)
viewMessage chunks =
    case chunks of
      [] ->
        [ text "\n\n\n" ]

      chunk :: others ->
        let
          htmlChunk =
            case chunk of
              Error.Unstyled string ->
                text string

              Error.Styled style string ->
                span (styleToAttrs style) [ text string ]
        in
        htmlChunk :: viewMessage others


styleToAttrs : Error.Style -> List (Attribute msg)
styleToAttrs { bold, underline, color } =
  addBold bold <| addUnderline underline <| addColor color []


addBold : Bool -> List (Attribute msg) -> List (Attribute msg)
addBold bool attrs =
  if bool then
    style "font-weight" "bold" :: attrs
  else
    attrs


addUnderline : Bool -> List (Attribute msg) -> List (Attribute msg)
addUnderline bool attrs =
  if bool then
    style "text-decoration" "underline" :: attrs
  else
    attrs


addColor : Maybe Error.Color -> List (Attribute msg) -> List (Attribute msg)
addColor maybeColor attrs =
  case maybeColor of
    Nothing ->
      attrs

    Just color ->
      style "color" (colorToCss color) :: attrs


colorToCss : Error.Color -> String
colorToCss color =
  case color of
    Error.Red -> "rgb(194,54,33)"
    Error.RED -> "rgb(252,57,31)"
    Error.Magenta -> "rgb(211,56,211)"
    Error.MAGENTA -> "rgb(249,53,248)"
    Error.Yellow -> "rgb(27 126 205)"
    Error.YELLOW -> "rgb(234,236,35)"
    Error.Green -> "rgb(37,188,36)"
    Error.GREEN -> "rgb(40,139,32)"
    Error.Cyan -> "rgb(51,187,200)"
    Error.CYAN -> "rgb(20,240,240)"
    Error.Blue -> "rgb(73,46,225)"
    Error.BLUE -> "rgb(88,51,255)"
    Error.White -> "rgb(203,204,205)"
    Error.WHITE -> "rgb(233,235,235)"
    Error.Black -> "rgb(0,0,0)"
    Error.BLACK -> "rgb(129,131,131)"



-- NAVIGATION

viewNavigation : Model -> Html Msg
viewNavigation model =
  nav
    [ id "navigation"
    , classList
        [ ( "theme-light", model.isLight )
        , ( "theme-dark", not model.isLight )
        , ( "open", model.isMenuOpen )
        , ( "closed", not model.isMenuOpen )
        ]
    ]
    [ section
        [ id "topbar" ]
        [ aside []
            [ menuButton
                { icon = if model.isLight then "#moon" else "#sun"
                , iconColor = ""
                , label = Just (if model.isLight then "Lights off" else "Lights on")
                , alt = "Switch the color scheme"
                , onClick = OnToggleLights
                }

            --, viewExamplesLink
              --menuButton
              --  { icon = if model.isMenuOpen then "#down" else "#up"
              --  , iconColor = ""
              --  , label = Just "More examples"
              --  , alt = if model.isMenuOpen then "Close menu" else "Open menu"
              --  , onClick = OnToggleMenu
              --  }

            , case model.token of
                Nothing ->
                  text ""

                Just token ->
                  lazy2 viewHint token model.table
            ]

        , aside []
            [ menuButton
                { icon = "#refresh"
                , iconColor = "blue"
                , label = Just "Check changes"
                , alt = "Compile your code (Ctrl-Enter)"
                , onClick = OnCompile
                }
            , menuButton
                { icon = "#chain"
                , iconColor = ""
                , label = Just "Share"
                , alt = "Compile your code (Ctrl-Enter)"
                , onClick = OnCompile
                }
            , menuButton
                { icon = "#send"
                , iconColor = ""
                , label = Just "Deploy"
                , alt = "Compile your code (Ctrl-Enter)"
                , onClick = OnCompile
                }
            ]
        ]
    ]


menuButton :
  { icon : String
  , iconColor : String
  , label : Maybe String
  , alt : String
  , onClick : msg
  }
  -> Html msg
menuButton config =
   button
    [ attribute "aria-label" config.alt, onClick config.onClick ] <|
    case config.label of
      Just label -> [ icon config.iconColor config.icon, span [] [ text label ] ]
      Nothing -> [ icon config.iconColor config.icon ]


icon : String -> String -> Html msg
icon colorClass name =
  svg [ SA.class ("icon " ++ colorClass) ] [ use [ xlinkHref name ] [] ]



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

