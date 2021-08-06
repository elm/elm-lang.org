module Errors exposing (..)

{-| The formatting of compilation errors.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Elm.Error as Error


view : (Error.Region -> msg) -> Error.Error -> Html msg
view onJumpToProblem error =
  case error of
    Error.GeneralProblem problem ->
      div
        [ id "errors" ]
        [ viewContainer
            [ viewHeader [ viewTitle problem.title, viewModuleName problem.path ]
            , viewBody problem.message
            ]
        ]

    Error.ModuleProblems modules ->
      let viewModuleError module_ =
            div [ class "error-module" ] (List.map viewProblem module_.problems)

          viewProblem problem =
            viewContainer
              [ viewHeader [ viewTitle problem.title, viewRegion onJumpToProblem problem.region ]
              , viewBody problem.message
              ]
      in
      div [ id "errors" ] (List.map viewModuleError modules)


viewContainer : List (Html msg) -> Html msg
viewContainer =
  div [ class "error-container" ]


viewHeader : List (Html msg) -> Html msg
viewHeader =
  div [ class "error-header" ]


viewTitle : String -> Html msg
viewTitle title =
  div [ class "error-title"] [ text title ]


viewRegion : (Error.Region -> msg) -> Error.Region -> Html msg
viewRegion onJumpToProblem region =
  div [ class "error-region", onClick (onJumpToProblem region) ] [ text "Jump to problem" ]


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

