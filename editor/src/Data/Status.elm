module Data.Status exposing
  ( Status(..)
  , getProblems, hasProblems
  , changed, compiling, success, problems
  )


import Json.Encode as E
import Json.Decode as D
import Elm.Error as Error
import Data.Problem as Problem exposing (Problem)


type Status
  = Changed
  | Compiling
  | Success
  | HasProblems (List Problem)
  | HasProblemsButChanged (List Problem)
  | HasProblemsButRecompiling (List Problem)
  | Failed String


getProblems : Status -> List Problem
getProblems status =
  case status of
    Changed                       -> []
    Compiling                     -> []
    Success                       -> []
    Failed _                      -> []
    HasProblems pbs               -> pbs
    HasProblemsButChanged pbs     -> pbs
    HasProblemsButRecompiling pbs -> pbs


hasProblems : Status -> Bool
hasProblems status =
  case status of
    Changed                       -> False
    Compiling                     -> False
    Success                       -> False
    Failed _                      -> False
    HasProblems _                 -> True
    HasProblemsButChanged _       -> True
    HasProblemsButRecompiling _   -> True


changed : Status -> Status
changed status =
  case status of
    Changed                       -> Changed
    Compiling                     -> Changed
    Success                       -> Changed
    Failed _                      -> Changed
    HasProblems pbs               -> HasProblemsButChanged pbs
    HasProblemsButChanged pbs     -> HasProblemsButChanged pbs
    HasProblemsButRecompiling pbs -> HasProblemsButChanged pbs


compiling : Status -> Status
compiling status =
  case status of
    Changed                       -> Compiling
    Compiling                     -> Compiling
    Success                       -> Compiling
    Failed _                      -> Compiling
    HasProblems pbs               -> HasProblemsButRecompiling pbs
    HasProblemsButChanged pbs     -> HasProblemsButRecompiling pbs
    HasProblemsButRecompiling pbs -> HasProblemsButRecompiling pbs


success : Status
success =
  Success


problems : E.Value -> Status
problems value =
  case D.decodeValue Error.decoder value of
    Ok errors ->
      HasProblems (Problem.toIndexedProblems errors)

    Err _ ->
      Failed "Could not decode compilation problems." -- TODO

