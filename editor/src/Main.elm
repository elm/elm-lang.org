port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Deps
import Header
import Hint
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (xlinkHref)
import Http



-- PORTS


port submissions : (String -> msg) -> Sub msg
port cursorMoves : (Maybe String -> msg) -> Sub msg
port gotLights : (Bool -> msg) -> Sub msg
port importEndLines : Int -> Cmd msg
port compile : () -> Cmd msg
port toggleLights : () -> Cmd msg



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
  , isLight : Bool
  , isMenuOpen : Bool
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
      ( { token = Nothing
        , table = Hint.defaultTable
        , imports = Header.defaultImports
        , dependencies = Loading
        , isLight = True
        , isMenuOpen = False
        }
      , fetchDepsInfo
      )

    Just ( imports, importEnd ) ->
      ( { token = Nothing
        , table = Hint.defaultTable
        , imports = imports
        , dependencies = Loading
        , isLight = True
        , isMenuOpen = False
        }
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
  | OnCompile
  | OnToggleLights
  | GotLights Bool
  | OnToggleMenu


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

    OnCompile ->
      ( model, compile () )

    OnToggleLights ->
      ( model, toggleLights () )

    GotLights isLight ->
      ( { model | isLight = isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ submissions Submitted
    , cursorMoves CursorMoved
    , gotLights GotLights
    ]



-- VIEW


view : Model -> Html Msg
view model =
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
            [ viewExamplesLink
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
                { icon = if model.isLight then "#moon" else "#sun"
                , iconColor = ""
                , label = Just (if model.isLight then "Lights off" else "Lights on")
                , alt = "Switch the color scheme"
                , onClick = OnToggleLights
                }
            , menuButton
                { icon = "#refresh"
                , iconColor = "blue"
                , label = Just "Check changes"
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

