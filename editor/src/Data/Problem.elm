module Data.Problem exposing (..)

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



