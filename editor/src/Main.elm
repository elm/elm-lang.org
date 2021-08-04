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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
      ( model, submitSource model.source )

    OnToggleLights ->
      ( { model | isLight = not model.isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

    GotErrors v ->
      ( model, Cmd.none )




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

    , node "split-page"
        [ on "move" (D.map OnMoveSplit (D.at [ "target", "split" ] D.float))
        , property "split" (E.float model.split)
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

        , iframe [ id "output", name "output", src ("/examples/_compiled/" ++ model.name ++ ".html") ] []
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

