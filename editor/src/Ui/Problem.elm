module Ui.Problem exposing (..)

{-| The formatting of compilation errors.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Elm.Error as Error
import Navigation
import FeatherIcons as I
import Data.Problem exposing (..)



type alias Config msg =
  { onJump : Error.Region -> msg
  , onPrevious : msg
  , onNext : msg
  , onMinimize : msg
  }


viewCarousel : Config msg -> Problems -> Html msg
viewCarousel config problems =
  let focused =
        getFocused problems
  in
  div
    [ id "errors" ]
    [ viewContainer
        [ viewHeader
            [ viewTitle focused.title
            , nav []
                [ viewLocation config.onJump focused.location
                , Navigation.iconButton []
                    { icon = I.chevronLeft
                    , iconColor = Nothing
                    , label = Nothing
                    , alt = "See previous problem"
                    , onClick = if hasPrevious problems then Nothing else Just config.onPrevious
                    }
                , Navigation.iconButton []
                    { icon = I.chevronRight
                    , iconColor = Nothing
                    , label = Nothing
                    , alt = "See next problem"
                    , onClick = if hasNext problems then Nothing else Just config.onNext
                    }
                , Navigation.iconButton []
                    { icon = I.minimize2
                    , iconColor = Nothing
                    , label = Nothing
                    , alt = "Minimize problem view"
                    , onClick = Just config.onMinimize
                    }
                ]
            ]
        , viewBody focused.message
        ]
    ]


viewCarouselMini : Config msg -> Problems -> Html msg
viewCarouselMini config problems =
  let focused =
        getFocused problems
  in
  div
    [ id "errors-mini" ]
    [ viewLocation config.onJump focused.location
    , a [ onClick config.onMinimize ] [ text (getSummary focused) ]
    , nav []
        [ Navigation.iconButton []
            { icon = I.chevronLeft
            , iconColor = Nothing
            , label = Nothing
            , alt = "See previous problem"
            , onClick = if hasPrevious problems then Nothing else Just config.onPrevious
            }
        , Navigation.iconButton []
            { icon = I.chevronRight
            , iconColor = Nothing
            , label = Nothing
            , alt = "See next problem"
            , onClick = if hasNext problems then Nothing else Just config.onNext
            }
        ]
    ]


viewList : (Error.Region -> msg) -> Problems -> Html msg
viewList onJumpToProblem problems =
  let viewProblem problem =
        viewContainer
          [ viewHeader
              [ viewTitle problem.title
              , nav []
                  [ viewLocation onJumpToProblem problem.location ]
              ]
          , viewBody problem.message
          ]
  in
  div [ id "errors" ] (List.map viewProblem (getAll problems))


viewContainer : List (Html msg) -> Html msg
viewContainer =
  div [ class "error-container" ]


viewHeader : List (Html msg) -> Html msg
viewHeader =
  div [ class "error-header" ]


viewTitle : String -> Html msg
viewTitle title =
  div [ class "error-title"] [ text title ]


viewLocation : (Error.Region -> msg) -> Location -> Html msg
viewLocation onJumpToProblem location =
  case location of
    General { path } -> viewModuleName path
    Specific { path, name, region } -> viewRegion onJumpToProblem name region


viewRegion : (Error.Region -> msg) -> String -> Error.Region -> Html msg
viewRegion onJumpToProblem name region =
  a [ class "error-region", onClick (onJumpToProblem region) ]
    [ text (name ++ ":" ++ String.fromInt region.start.line) ]


viewModuleName : Maybe String -> Html msg
viewModuleName name =
  div [ class "error-region"] [ text (Maybe.withDefault "" name) ]


viewBody : List Error.Chunk -> Html msg
viewBody =
  div [ class "error-body" ] << viewChunks


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
      [ ( "error-bold", bold )
      , ( "error-underline", underline )
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

