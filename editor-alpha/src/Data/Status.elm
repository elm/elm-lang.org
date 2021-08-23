module Data.Status exposing
  ( Status(..)
  , getProblems, hasProblems, withProblems, isCompiling
  , changed, compiling, success, problems
  )


import Json.Encode as E
import Json.Decode as D
import Elm.Error as Error
import Data.Problem as Problem exposing (Problem, Problems)


type Status
  = Changed
  | Compiling
  | Success
  | HasProblems Problems
  | HasProblemsButChanged Problems
  | HasProblemsButRecompiling Problems
  | Failed String


getProblems : Status -> Maybe Problems
getProblems status =
  case status of
    Changed                       -> Nothing
    Compiling                     -> Nothing
    Success                       -> Nothing
    Failed _                      -> Nothing
    HasProblems pbs               -> Just pbs
    HasProblemsButChanged pbs     -> Just pbs
    HasProblemsButRecompiling pbs -> Just pbs


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


withProblems : Status -> (Problems -> Problems) -> Status
withProblems status func =
  case status of
    Changed                       -> Changed
    Compiling                     -> Compiling
    Success                       -> Success
    Failed msg                    -> Failed msg
    HasProblems pbs               -> HasProblems (func pbs)
    HasProblemsButChanged pbs     -> HasProblemsButChanged (func pbs)
    HasProblemsButRecompiling pbs -> HasProblemsButRecompiling (func pbs)


isCompiling : Status -> Bool
isCompiling status =
  case status of
    Changed                       -> False
    Compiling                     -> True
    Success                       -> False
    Failed _                      -> False
    HasProblems _                 -> False
    HasProblemsButChanged _       -> False
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
      case Problem.init (Problem.toIndexedProblems errors) of
        Just pbs -> HasProblems pbs
        Nothing -> Failed "Somehow returned zero problems."

    Err _ ->
      Failed "Could not decode compilation problems." -- TODO


