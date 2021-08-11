module Main exposing (main)


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
import Data.Status as Status
import Data.Problem as Problem
import Data.Window exposing (Window)
import Ui.Problem
import Ui.Navigation
import Ui.ColumnDivider
import Ui.Editor


-- TODO
-- Clean up styles
-- Make sure mini errors goes away when pane is moved in
-- Make state more exact
-- Make deps hints work again
-- See if hints can be optimized futher
-- add window resizing
-- fix current problem on recompile



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
  { name : String
  , window : Window
  , editor : Ui.Editor.Model
  , divider : Ui.ColumnDivider.Model
  , isLight : Bool
  , isMenuOpen : Bool
  , areProblemsMini : Bool
  , status : Status.Status
  }



-- INIT


init : { original : String, name : String, width : Float, height : Float } -> ( Model, Cmd Msg )
init flags =
  let ( editor, editorCmd ) =
        Ui.Editor.init flags.original

      window =
        { width = flags.width, height = flags.height }
  in
  ( { name = flags.name
    , window = window
    , editor = editor
    , divider = Ui.ColumnDivider.init window
    , isLight = True
    , isMenuOpen = False
    , areProblemsMini = False
    , status = Status.success
    }
  , Cmd.map OnEditorMsg editorCmd
  )



-- UPDATE


type Msg
  = OnEditorMsg Ui.Editor.Msg
  | OnDividerMsg Ui.ColumnDivider.Msg
  | OnPreviousProblem (Maybe Error.Region)
  | OnNextProblem (Maybe Error.Region)
  | OnJumpToProblem Error.Region
  | OnMinimizeProblem Bool
  | OnToggleLights
  | OnToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnEditorMsg subMsg ->
      let ( editor, status, editorCmd ) =
            Ui.Editor.update subMsg model.editor model.status
      in
      ( { model | editor = editor, status = status }
      , Cmd.map OnEditorMsg editorCmd
      )

    OnDividerMsg subMsg ->
      ( { model | divider = Ui.ColumnDivider.update model.window subMsg model.divider }
      , Cmd.none
      )

    OnPreviousProblem maybeRegion ->
      ( { model | status = Status.withProblems model.status Problem.focusPrevious }
          |> (Maybe.withDefault identity (Maybe.map jumpToRegion maybeRegion))
      , Cmd.none
      )

    OnNextProblem maybeRegion ->
      ( { model | status = Status.withProblems model.status Problem.focusNext }
          |> (Maybe.withDefault identity (Maybe.map jumpToRegion maybeRegion))
      , Cmd.none
      )

    OnJumpToProblem region ->
      ( jumpToRegion region model, Cmd.none )

    OnMinimizeProblem isMini ->
      ( { model | areProblemsMini = isMini }, Cmd.none )

    OnToggleLights ->
      ( { model | isLight = not model.isLight }, Cmd.none )

    OnToggleMenu ->
      ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )


jumpToRegion : Error.Region -> Model -> Model
jumpToRegion region model =
  { model | editor = Ui.Editor.setSelection region model.editor }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Ui.Editor.subscriptions model.editor
    |> Sub.map OnEditorMsg



-- VIEW


view : Model -> Html Msg
view model =
  let hasErrors =
        Status.hasProblems model.status
  in
  main_
    [ id "main"
    , classList
        [ ( "theme-light", model.isLight )
        , ( "theme-dark", not model.isLight )
        ]
    ]
    [ Ui.ColumnDivider.view OnDividerMsg model.window model.divider
        [ Ui.Editor.viewEditor model.isLight model.editor
            |> Html.map OnEditorMsg
        , case Status.getProblems model.status of
            Just problems ->
              div
                [ id "popup"
                , if Ui.ColumnDivider.isUpperLimit model.window model.divider
                  then style "transform" "translateY(0)"
                  else style "transform" "translateY(100%)"
                , if Ui.ColumnDivider.isUpperLimit model.window model.divider
                  then style "transition-delay" "0.5s;"
                  else style "transition-delay" "0s;"
                ]
                [ if model.areProblemsMini then
                    text ""
                  else
                    Ui.Problem.viewCarousel
                        { onJump = OnJumpToProblem
                        , onPrevious = OnPreviousProblem
                        , onNext = OnNextProblem
                        , onMinimize = OnMinimizeProblem True
                        }
                        problems
                ]

            Nothing ->
              text ""
        , viewNavigation model
        ]
        [ case Status.getProblems model.status of
            Just problems ->
              if Ui.ColumnDivider.isUpperLimit model.window model.divider then
                text ""
              else
                Ui.Problem.viewList OnJumpToProblem problems

            Nothing ->
              text ""
        , iframe
            [ id "output"
            , name "output"
            , if Status.hasProblems model.status
              then style "display" "none"
              else style "display" "block"
            , src ("/examples/_compiled/" ++ model.name ++ ".html")
            ]
            []
        ]
    ]



-- NAVIGATION


viewNavigation : Model -> Html Msg
viewNavigation model =
  Ui.Navigation.view
    { isLight = model.isLight
    , isOpen = model.isMenuOpen
    , left =
        [ Ui.Navigation.elmLogo
        , Ui.Navigation.lights OnToggleLights model.isLight
        , Ui.Editor.viewHint model.editor
        ]
    , right =
        [ case Status.getProblems model.status of
            Just problems ->
              if not model.areProblemsMini || not (Ui.ColumnDivider.isUpperLimit model.window model.divider) then
                text ""
              else
                Ui.Problem.viewCarouselMini
                    { onJump = OnJumpToProblem
                    , onPrevious = OnPreviousProblem
                    , onNext = OnNextProblem
                    , onMinimize = OnMinimizeProblem False
                    }
                    problems

            Nothing ->
              text ""
        , Ui.Navigation.compilation (OnEditorMsg Ui.Editor.OnCompile) model.status
        ]
    }


