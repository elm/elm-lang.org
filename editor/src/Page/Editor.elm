module Page.Editor exposing (main)


import Browser
import Browser.Events
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
import Data.Window as Window exposing (Window)
import Data.Version exposing (Version)
import Data.Analytics as Analytics
import Data.Registry.Defaults as Defaults
import Data.Registry.Package as Package
import Ui.Problem
import Ui.Navigation
import Ui.ColumnDivider
import Ui.Editor
import Ui.Package
import Dict exposing (Dict)



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
  , packageUi : Ui.Package.Model
  , isPackageUiOpen : Bool
  }


getProblems : Model -> Maybe Problem.Problems
getProblems model =
  case ( Ui.Package.getProblems model.packageUi, Status.getProblems model.status ) of
    ( Just problems, _ ) ->
      Just problems

    ( Nothing, Just problems ) ->
      Just problems

    ( Nothing, Nothing ) ->
      Nothing



-- INIT


type alias Flags =
  { original : String
  , name : String
  , width : Int
  , height : Int
  , direct : List Package.Package
  , indirect : List Package.Package
  }


decodeFlags : D.Decoder Flags
decodeFlags =
  D.map6 Flags
    (D.field "original" D.string)
    (D.field "name" D.string)
    (D.field "width" D.int)
    (D.field "height" D.int)
    (D.at [ "dependencies", "direct"] Defaults.decode)
    (D.at [ "dependencies", "indirect"] Defaults.decode)


defaultFlags : Flags
defaultFlags =
  { original = "try"
  , name = "try"
  , width = 1000
  , height = 700
  , direct = Defaults.direct
  , indirect = Defaults.indirect
  }



init : E.Value -> ( Model, Cmd Msg )
init flagsRaw =
  let flags =
        Result.withDefault defaultFlags (D.decodeValue decodeFlags flagsRaw)

      ( editor, editorCmd ) =
        Ui.Editor.init flags.original

      ( packageUi, packageUiCmd ) =
        Ui.Package.init flags.direct flags.indirect

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
    , status = Status.changed Status.success
    , packageUi = packageUi
    , isPackageUiOpen = False
    }
  , Cmd.batch
      [ Cmd.map OnEditorMsg editorCmd
      , Cmd.map OnPackageMsg packageUiCmd
      ]
  )



-- UPDATE


type Msg
  = OnEditorMsg Ui.Editor.Msg
  | OnDividerMsg Ui.ColumnDivider.Msg
  | OnPackageMsg Ui.Package.Msg
  | OnPreviousProblem (Maybe Error.Region)
  | OnNextProblem (Maybe Error.Region)
  | OnJumpToProblem Error.Region
  | OnMinimizeProblem Bool
  | OnToggleLights
  | OnToggleMenu
  | OnTogglePackages
  | OnWindowSize Int Int
  | OnReportResult (Result Http.Error String)
  | OnJsError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnEditorMsg subMsg ->
      let ( editor, status, editorCmd ) =
            Ui.Editor.update subMsg model.editor model.status

          packageUi =
            if Status.isCompiling status then
              Ui.Package.dismissAll model.packageUi
            else
              model.packageUi
      in
      ( { model | editor = editor, status = status, packageUi = packageUi }
      , Cmd.batch
          [ Cmd.map OnEditorMsg editorCmd
          , case status of
              Status.Failed errMsg ->
                Analytics.reportError OnReportResult errMsg

              _ ->
                Cmd.none
          ]
      )

    OnDividerMsg subMsg ->
      ( { model | divider = Ui.ColumnDivider.update model.window subMsg model.divider }
      , Cmd.none
      )

    OnPackageMsg subMsg ->
      let ( packageUi, shouldRebuild, packageUiCmd ) =
            Ui.Package.update subMsg model.packageUi
      in
      ( { model | packageUi = packageUi
        , status = if shouldRebuild then Status.changed model.status else model.status
        }
      , Cmd.map OnPackageMsg packageUiCmd
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

    OnTogglePackages ->
      let isOpenNow = not model.isPackageUiOpen
          push = if isOpenNow then Ui.ColumnDivider.pushLeft else Ui.ColumnDivider.pushRight
      in
      ( { model | isPackageUiOpen = isOpenNow
        , divider = push model.window Ui.Package.width model.divider
        }
      , Cmd.none
      )

    OnWindowSize width height ->
      ( { model | window = { width = width, height = height } }, Cmd.none )

    OnReportResult _ ->
      ( model, Cmd.none )

    OnJsError errMsg ->
      ( model, Analytics.reportError OnReportResult errMsg )


jumpToRegion : Error.Region -> Model -> Model
jumpToRegion region model =
  { model | editor = Ui.Editor.setSelection region model.editor }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Ui.Editor.subscriptions model.editor
        |> Sub.map OnEditorMsg
    , Browser.Events.onResize OnWindowSize
    , Analytics.gotJsError OnJsError
    ]



-- VIEW


view : Model -> Html Msg
view model =
  let packageStyles =
        if model.isPackageUiOpen then
          if Window.isLessThan model.window Ui.Package.width
          then [ style "max-width" Ui.Package.widthPx, style "border" "0" ]
          else [ style "max-width" Ui.Package.widthPx ]
        else
          [ style "max-width" "0", style "border" "0" ]
  in
  main_
    [ id "main"
    , classList
        [ ( "theme-light", model.isLight )
        , ( "theme-dark", not model.isLight )
        ]
    ]
    [ Ui.ColumnDivider.view OnDividerMsg model.window model.divider
        [ Ui.Package.view packageStyles model.packageUi
            |> Html.map OnPackageMsg

        , Ui.Editor.viewEditor (Ui.Package.getSolution model.packageUi) model.isLight model.editor
            |> Html.map OnEditorMsg

        , case getProblems model of
            Just problems ->
              div
                [ id "problems-carousel"
                , if Ui.ColumnDivider.isRightMost model.window model.divider
                  then style "transform" "translateX(0)"
                  else style "transform" "translateX(100%)"
                ]
                [ if model.areProblemsMini then
                    text ""
                  else
                    lazy viewProblemPopup problems
                ]

            Nothing ->
              text ""

        , viewNavigation model
        ]
        [ case getProblems model of
            Just problems ->
              if Ui.ColumnDivider.isRightMost model.window model.divider then
                text ""
              else
                lazy viewProblemList problems

            Nothing ->
              text ""

        , iframe
            [ id "output"
            , name "output"
            , case getProblems model of
                Just _  -> style "display" "none"
                Nothing -> style "display" "block"
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
        , Ui.Navigation.packages OnTogglePackages model.isPackageUiOpen
        , Ui.Editor.viewHint model.editor
        ]
    , right =
        [ case getProblems model of
            Just problems ->
              if not model.areProblemsMini || not (Ui.ColumnDivider.isRightMost model.window model.divider) then
                text ""
              else
                lazy viewProblemMini problems

            Nothing ->
              text ""
        , Ui.Navigation.compilation (OnEditorMsg Ui.Editor.OnCompile) model.status
        --, Ui.Navigation.share (OnEditorMsg Ui.Editor.OnCompile)
        --, Ui.Navigation.deploy (OnEditorMsg Ui.Editor.OnCompile)
        ]
    }


-- PROBLEMS


viewProblemPopup : Problem.Problems -> Html Msg
viewProblemPopup =
  Ui.Problem.viewCarousel
    { onJump = OnJumpToProblem
    , onPrevious = OnPreviousProblem
    , onNext = OnNextProblem
    , onMinimize = OnMinimizeProblem True
    }


viewProblemMini : Problem.Problems -> Html Msg
viewProblemMini =
  Ui.Problem.viewCarouselMini
    { onJump = OnJumpToProblem
    , onPrevious = OnPreviousProblem
    , onNext = OnNextProblem
    , onMinimize = OnMinimizeProblem False
    }


viewProblemList :  Problem.Problems -> Html Msg
viewProblemList =
  Ui.Problem.viewList OnJumpToProblem

