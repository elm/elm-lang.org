module Errors exposing (..)

{-| The formatting of compilation errors.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Elm.Error as Error
import Navigation
import FeatherIcons as I



-- PROCESSING


type alias Problem =
  { index : Int
  , location : Location
  , title : String
  , message : List Error.Chunk
  }


type Location
  = General { path : Maybe String }
  | Specific { path : String, name : String, region : Error.Region }


toIndexedProblems : Error.Error -> List  Problem
toIndexedProblems errors =
  case errors of
    Error.GeneralProblem problem ->
      [ { index = 0
        , location = General { path = problem.path }
        , title = problem.title
        , message = problem.message
        }
      ]

    Error.ModuleProblems modules ->
      let toModuleProblems module_ ( nextIndex, all ) =
            List.foldr (toSingleProblem module_) ( nextIndex, all ) module_.problems

          toSingleProblem module_ problem ( nextIndex, all ) =
            let indexedProblem =
                  { index = nextIndex
                  , location =
                      Specific
                        { path = module_.path
                        , name = module_.name
                        , region = problem.region
                        }
                  , title = problem.title
                  , message = problem.message
                  }
            in
            ( nextIndex + 1, indexedProblem :: all )
      in
      List.foldr toModuleProblems ( 0, [] ) modules
        |> Tuple.second


getSummary : Problem -> String
getSummary problem =
  case problem.message of
      [] ->
        ""

      chunk :: others ->
        let getFirstLine str =
              let firstLine = List.head (String.split "\n" str)
                  firstColon = List.head (String.split ":" str)
              in
              case ( firstLine, firstColon ) of
                ( Just firstLine_, Just firstColon_ ) ->
                  if String.length firstLine_ > String.length firstColon_ then firstColon_ else firstLine_

                ( Just firstLine_, Nothing ) ->
                  firstLine_

                ( Nothing, Just firstColon_ ) ->
                  firstColon_

                ( Nothing, Nothing ) ->
                  "Click to see full error message!"
        in
        case chunk of
          Error.Unstyled string ->
            getFirstLine string

          Error.Styled style string ->
            getFirstLine string -- TODO



-- VIEW


type alias Config msg =
  { onJump : Error.Region -> msg
  , onProblem : Int -> msg
  , onMinimize : msg
  , current : Int
  }


viewCurrent : Config msg -> (Config msg -> Int -> Problem -> Html msg) -> List Problem -> Html msg
viewCurrent config view problems =
  let total =
        List.length problems

      isCurrent problem =
        problem.index == config.current
  in
  case List.head (List.filter isCurrent problems) of
      Nothing -> text ""
      Just problem -> view config total problem


viewCarousel : Config msg -> Int -> Problem -> Html msg
viewCarousel config total problem =
  div
    [ id "errors" ]
    [ viewContainer
        [ viewHeader
            [ viewTitle problem.title
            , nav []
                [ viewLocation config.onJump problem.location
                , Navigation.iconButton []
                    { icon = I.chevronLeft
                    , iconColor = Nothing
                    , label = Nothing
                    , alt = "See previous problem"
                    , onClick =
                        if config.current - 1 < 0 then Nothing
                        else Just <| config.onProblem (config.current - 1)
                    }
                , Navigation.iconButton []
                    { icon = I.chevronRight
                    , iconColor = Nothing
                    , label = Nothing
                    , alt = "See next problem"
                    , onClick =
                        if config.current + 1 >= total then Nothing
                        else Just <| config.onProblem (config.current + 1)
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
        , viewBody problem.message
        ]
    ]


viewCarouselMini : Config msg -> Int -> Problem -> Html msg
viewCarouselMini config total problem =
  div
    [ id "errors-mini" ]
    [ viewLocation config.onJump problem.location
    , a [ onClick config.onMinimize ] [ text (getSummary problem) ]
    , nav []
        [ Navigation.iconButton []
            { icon = I.chevronLeft
            , iconColor = Nothing
            , label = Nothing
            , alt = "See previous problem"
            , onClick =
                if config.current - 1 < 0 then Nothing
                else Just <| config.onProblem (config.current - 1)
            }
        , Navigation.iconButton []
            { icon = I.chevronRight
            , iconColor = Nothing
            , label = Nothing
            , alt = "See next problem"
            , onClick =
                if config.current + 1 >= total then Nothing
                else Just <| config.onProblem (config.current + 1)
            }
        ]
    ]


viewList : (Error.Region -> msg) -> List Problem -> Html msg
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
  div [ id "errors" ] (List.map viewProblem problems)


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

