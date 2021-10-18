module Data.Registry.Solution exposing
  ( Solution, empty, toSolution, decoder, encode )


import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Data.Version as V
import Data.Registry.Package as Package
import Data.Registry.Defaults as Defaults
import Data.Registry.Status as Status


type alias Solution =
  { elm : V.Version
  , direct : Dict Package.Key V.Version
  , indirect : Dict Package.Key V.Version
  , hash : Maybe String
  }


empty : Solution
empty =
  Solution (V.Version 0 19 1) Dict.empty Dict.empty Nothing



-- API


toSolution : List ( Package.Package, Status.Status ) -> Solution
toSolution packages =
  let addToSolution ( package, state ) solution =
        case state of
          Status.DirectDep version ->
            { solution | direct = Dict.insert (Package.toKey package) version solution.direct }

          Status.IndirectDep version ->
            { solution | indirect = Dict.insert (Package.toKey package) version solution.indirect }

          _ ->
            solution
  in
  List.foldl addToSolution empty packages



-- DECODE / ENCODE


decoder : JD.Decoder Solution
decoder =
  JD.map4 Solution
    (JD.field "elm-version" V.decoder)
    (JD.field "direct" decodeDeps)
    (JD.field "indirect" decodeDeps)
    (JD.field "hash" (JD.nullable JD.string))


decodeDeps : JD.Decoder (Dict Package.Key V.Version)
decodeDeps =
  let shape dict =
        Dict.toList dict
          |> List.filterMap onlyValid
          |> Dict.fromList

      onlyValid ( name, version ) =
        Package.keyFromName name
          |> Maybe.map (\key -> ( key, version ))
  in
  JD.map shape (JD.dict V.decoder)


encode : Solution -> JE.Value
encode solution =
  JE.object
    [ ( "elm-version", V.encode solution.elm )
    , ( "direct", JE.dict Package.nameFromKey V.encode solution.direct )
    , ( "indirect", JE.dict Package.nameFromKey V.encode solution.indirect )
    , ( "hash", Maybe.withDefault JE.null (Maybe.map JE.string solution.hash) )
    ]
