module Data.Problem exposing
  ( Problem, Location(..)
  , toIndexedProblems, toManyIndexedProblems
  , reindex
  , getRegion
  , generic
  , Problems, init, merge
  , getAll, getFocused
  , getPrevious, getNext
  , hasPrevious, hasNext
  , focusPrevious, focusNext
  , getSummary, getPlainText
  )

{-| The formatting of compilation errors.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Elm.Error as Error
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


toIndexedProblems : Error.Error -> List Problem
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


toManyIndexedProblems : List Error.Error -> List Problem
toManyIndexedProblems errors =
  reindex <| List.concatMap toIndexedProblems errors


reindex : List Problem -> List Problem
reindex =
  List.indexedMap (\i p -> { p | index = i } )


getRegion : Problem -> Maybe Error.Region
getRegion problem =
  case problem.location of
    General _ -> Nothing
    Specific specific -> Just specific.region



-- GENERIC ERROR


generic : Problem
generic =
  { index = 0
  , location = General { path = Nothing }
  , title = "INTERNAL ERROR"
  , message =
      [ Error.Unstyled "Oops, something went wrong.\n\nThe problem has been reported and will be addressed "
      , Error.Unstyled "as soon as possible. Please\ntry again later!"
      ]
  }


-- MANY


type alias Problems =
  ( List Problem, Problem, List Problem )


init : List Problem -> Maybe Problems
init pbs =
  case pbs of
    first :: rest -> Just ( [], first, rest )
    _ -> Nothing


merge : Problems -> Problems -> Problems
merge ( prevA, currA, nextA ) pbs =
  ( prevA, currA, nextA ++ getAll pbs )


getAll : Problems -> List Problem
getAll ( prev, curr, next ) =
  List.reverse prev ++ [ curr ] ++ next


getFocused : Problems -> Problem
getFocused ( _, current, _ ) =
  current


getPrevious : Problems -> Maybe Problem
getPrevious ( prev, _, _ ) =
  case prev of
    first :: _ -> Just first
    [] -> Nothing


getNext : Problems -> Maybe Problem
getNext ( _, _, next ) =
  case next of
    first :: _ -> Just first
    [] -> Nothing


hasPrevious : Problems -> Bool
hasPrevious ( prev, curr, next ) =
  not (List.isEmpty prev)


hasNext : Problems -> Bool
hasNext ( prev, curr, next ) =
  not (List.isEmpty next)


focusNext : Problems -> Problems
focusNext pbs =
  case pbs of
    ( prev, curr, next :: remaining ) -> ( curr :: prev, next, remaining )
    ( prev, curr, [] ) -> pbs


focusPrevious : Problems -> Problems
focusPrevious pbs =
  case pbs of
    ( prev :: remaining, curr, next ) -> ( remaining, prev, curr :: next )
    ( [], curr, next ) -> pbs



-- HELPERS


getSummary : Problem -> String
getSummary problem =
  let plainText = getPlainText problem
      firstLine = List.head (String.split "\n\n" plainText)
      firstColon = List.head (String.split ":" plainText)
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


getPlainText : Problem -> String
getPlainText problem =
  let toPlain chunks =
        case chunks of
          [] ->
            ""

          chunk :: rest ->
            case chunk of
              Error.Unstyled string ->
                string ++ toPlain rest

              Error.Styled _ string ->
                string ++ toPlain rest
  in
  toPlain problem.message

