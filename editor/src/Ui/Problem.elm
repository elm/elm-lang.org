module Ui.Problem exposing (..)

{-| The formatting of compilation problems.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Elm.Error as Error
import Ui.Navigation
import FeatherIcons as I
import Data.Problem exposing (..)



type alias Config msg =
  { onJump : Error.Region -> msg
  , onPrevious : Maybe Error.Region -> msg
  , onNext : Maybe Error.Region -> msg
  , onMinimize : msg
  }


viewCarousel : Config msg -> Problems -> Html msg
viewCarousel config problems =
  let focused =
        getFocused problems

      previousRegion =
        Maybe.andThen getRegion (getPrevious problems)

      nextRegion =
        Maybe.andThen getRegion (getNext problems)
  in
  div
    [ id "problems" ]
    [ viewContainer
        [ viewHeader
            [ viewTitle focused.title
            , viewNavigation
                [ viewLocation config.onJump focused.location
                , viewPreviousButton (config.onPrevious previousRegion) problems
                , viewNextButton (config.onNext nextRegion) problems
                , viewMinimize config.onMinimize
                ]
            ]
        , viewBody focused.message
        ]
    ]


viewCarouselMini : Config msg -> Problems -> Html msg
viewCarouselMini config problems =
  let focused =
        getFocused problems

      previousRegion =
        Maybe.andThen getRegion (getPrevious problems)

      nextRegion =
        Maybe.andThen getRegion (getNext problems)

      viewLocationMini =
        case focused.location of
          General _ ->
            text ""

          Specific specific ->
            a [ class "problem-region", onClick (config.onJump specific.region) ]
              [ text (String.fromInt specific.region.start.line ++ "| ") ]
  in
  div
    [ id "problems-mini" ]
    [ viewLocationMini
    , a [ onClick config.onMinimize ] [ text (getSummary focused) ]
    , viewNavigation
        [ viewPreviousButton (config.onPrevious previousRegion) problems
        , viewNextButton (config.onNext nextRegion) problems
        ]
    ]


viewPreviousButton : msg -> Problems -> Html msg
viewPreviousButton onPrevious problems =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = I.chevronLeft
    , iconColor = Nothing
    , label = Nothing
    , labelColor = Nothing
    , alt = "See previous problem"
    , onClick = if hasPrevious problems then Just onPrevious else Nothing
    }


viewNextButton : msg -> Problems -> Html msg
viewNextButton onNext problems =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = I.chevronRight
    , iconColor = Nothing
    , labelColor = Nothing
    , label = Nothing
    , alt = "See next problem"
    , onClick = if hasNext problems then Just onNext else Nothing
    }


viewMinimize : msg -> Html msg
viewMinimize onMinimize =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = I.minimize2
    , iconColor = Nothing
    , label = Nothing
    , labelColor = Nothing
    , alt = "Minimize problem view"
    , onClick = Just onMinimize
    }



-- AS LIST


viewList : (Error.Region -> msg) -> Problems -> Html msg
viewList onJumpToProblem problems =
  let viewProblem problem =
        viewContainer
          [ viewHeader
              [ viewTitle problem.title
              , viewNavigation [ viewLocation onJumpToProblem problem.location ]
              ]
          , viewBody problem.message
          ]
  in
  div [ id "problems" ] (List.map viewProblem (getAll problems))



-- PARTS


viewContainer : List (Html msg) -> Html msg
viewContainer =
  div [ class "problem-container" ]


viewHeader : List (Html msg) -> Html msg
viewHeader =
  div [ class "problem-header" ]


viewNavigation : List (Html msg) -> Html msg
viewNavigation =
  nav [ class "problem-navigation" ]


viewTitle : String -> Html msg
viewTitle title =
  div [ class "problem-title"] [ text title ]


viewLocation : (Error.Region -> msg) -> Location -> Html msg
viewLocation onJumpToProblem location =
  case location of
    General { path } -> viewModuleName path
    Specific { path, name, region } -> viewRegion onJumpToProblem name region


viewRegion : (Error.Region -> msg) -> String -> Error.Region -> Html msg
viewRegion onJumpToProblem name region =
  a [ class "problem-region", onClick (onJumpToProblem region) ]
    [ text "Jump to problem" ]


viewModuleName : Maybe String -> Html msg
viewModuleName name =
  div [ class "problem-region"] [ text (Maybe.withDefault "" name) ]


viewBody : List Error.Chunk -> Html msg
viewBody =
  div [ class "problem-body" ] << viewChunks


viewChunks : List Error.Chunk -> List (Html msg)
viewChunks chunks =
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
                span (styleToClasses style) [ text string ]
        in
        htmlChunk :: viewChunks others


styleToClasses : Error.Style -> List (Attribute msg)
styleToClasses { bold, underline, color } =
  [ classList
      [ ( "bold", bold )
      , ( "underline", underline )
      , ( Maybe.map colorToClass color
            |> Maybe.withDefault ""
        , True
        )
      ]
  ]


colorToClass : Error.Color -> String
colorToClass color =
  case color of
    Error.Red -> "red"
    Error.RED -> "red"
    Error.Magenta -> "magenta"
    Error.MAGENTA -> "magenta"
    Error.Yellow -> "yellow"
    Error.YELLOW -> "yellow"
    Error.Green -> "green"
    Error.GREEN -> "green"
    Error.Cyan -> "cyan"
    Error.CYAN -> "cyan"
    Error.Blue -> "blue"
    Error.BLUE -> "blue"
    Error.White -> "white"
    Error.WHITE -> "white"
    Error.Black -> "black"
    Error.BLACK -> "black"

