module Data.Registry exposing
  ( Registry
  , initial, fetch, fromNews
  , insert, update
  , setStatus, mapStatus
  , dismissAll
  , getValues, getErrors
  , filterStatus, filterKeys
  , fromSolution
  , search
  , Action(..), attemptEdit
  )

import Dict exposing (Dict)
import Data.Version as V
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Json.Encode as JE
import Json.Decode as JD
import Http
import Regex
import Data.Registry.Status as Status
import Data.Registry.Package as Package
import Data.Registry.Solution as Solution
import Data.Registry.Defaults as Defaults
import Data.Http
import Elm.Error as Error



type alias Registry =
  Dict Package.Key ( Package.Package, Status.Status )



-- INIT


initial : Registry
initial =
  Dict.empty
    |> insert (Status.DirectDep << .version) Defaults.direct
    |> insert (Status.IndirectDep << .version) Defaults.indirect


fetch : (Result Http.Error (List Package.Package) -> msg) -> Cmd msg
fetch onResult =
  Http.get
    { url = "https://elm.studio/api/packages/all"
    , expect = Http.expectBytes onResult decoder
    }


fromNews : List Package.Package -> Registry -> Registry
fromNews news registry =
  let toStatus maybeStatus _ =
        case maybeStatus of
          Just state -> state
          Nothing -> Status.NotInstalled
  in
  update toStatus news registry



-- API


insert : (Package.Package -> Status.Status) -> List Package.Package -> Registry -> Registry
insert toStatus =
  update (always toStatus)


update : (Maybe Status.Status -> Package.Package -> Status.Status) -> List Package.Package -> Registry -> Registry
update toStatus packages registry =
  let fold package =
        Dict.update (Package.toKey package) (updateOne package)

      updateOne package maybeValue =
        Just ( package, toStatus (Maybe.map Tuple.second maybeValue) package )
  in
  List.foldl fold registry packages


mapStatus : (Package.Package -> Status.Status -> Status.Status) -> Registry -> Registry
mapStatus toStatus registry =
  let func key ( package, state ) =
        ( package, toStatus package state )
  in
  Dict.map func registry


setStatus : Package.Package -> Status.Status -> Registry -> Registry
setStatus pkg state =
  Dict.insert (Package.toKey pkg) ( pkg, state )


dismissAll : Registry -> Registry
dismissAll =
  Dict.map <| \_ ( pkg, state ) ->
    case state of
      Status.Failed _ -> ( pkg, Status.NotInstalled )
      _ -> ( pkg, state )


getValues : Registry -> List ( Package.Package, Status.Status )
getValues registry =
  Dict.values registry


getErrors : Registry -> List Status.Error
getErrors registry =
  List.filterMap (Status.getError << Tuple.second) (Dict.values registry)


filterStatus : (Status.Status -> Bool) -> Registry -> Registry
filterStatus inGroup =
  Dict.filter (\key ( pkg, state ) -> inGroup state)


filterKeys : List Package.Key -> Registry -> List ( Package.Package, Status.Status )
filterKeys searched registry =
  List.filterMap (\s -> Dict.get s registry) searched


fromSolution : Solution.Solution -> Registry -> Registry
fromSolution solution registry =
  let resetStatus state =
        case state of
          Status.Loading       -> state
          Status.NotInstalled  -> state
          Status.DirectDep _   -> Status.NotInstalled
          Status.IndirectDep _ -> Status.NotInstalled
          Status.Failed _      -> state

      insertDeps toStatus deps reg =
        List.foldl (fold toStatus) reg (Dict.toList deps)

      fold toStatus ( key, version ) =
        Dict.update key <| \maybePkg ->
          case maybePkg of
            Just ( pkg, _ ) ->
              Just ( pkg, toStatus version )

            Nothing ->
              Nothing
  in
  registry
    |> mapStatus (always resetStatus)
    |> insertDeps Status.DirectDep solution.direct
    |> insertDeps Status.IndirectDep solution.indirect


search : String -> List ( Package.Package, Status.Status ) -> List ( Package.Package, Status.Status )
search query packages =
  let regex =
        String.split "" query
          |> List.intersperse ".*"
          |> String.concat
          |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
          |> Maybe.withDefault Regex.never

      match ( pkg, _ ) =
        Regex.contains regex (Package.toName pkg)

      queryLower =
        String.toLower query

      toNameLower pkg =
        String.toLower (Package.toName pkg)

      order ( pkg, _ ) =
        if queryLower == pkg.project then 0 else
        if queryLower == pkg.author then 1 else
        if String.contains queryLower (toNameLower pkg) then 2
        else 3
  in
  packages
    |> List.filter match
    |> List.sortBy order



-- EDIT


type Action
  = Install
  | Uninstall


attemptEdit : Action -> (Result Status.Error Solution.Solution -> msg) -> Registry -> Package.Package -> Cmd msg
attemptEdit action toMsg registry package =
  let solution =
        Solution.toSolution (Dict.values registry)

      payload =
        JE.object
          [ ( "solution", Solution.encode solution )
          , ( encodeAction, encodePackage )
          ]

      encodeAction =
        case action of
          Install -> "install"
          Uninstall -> "uninstall"

      encodePackage =
        JE.object
          [ ( "package", JE.string (Package.toName package) )
          , ( "version", V.encode package.version )
          ]
  in
  Http.riskyRequest
    { method = "POST"
    , headers = []
    , url = "https://elm.studio/api/packages/edit"
    , body = Http.jsonBody payload
    , expect = Data.Http.expectJson toMsg Error.decoder Solution.decoder
    , timeout = Nothing
    , tracker = Nothing
    }



-- DECODER / REGISTRY


decoder : D.Decoder (List Package.Package)
decoder =
  decodeListLength
    |> D.andThen (\len -> D.loop ( len, [] ) listStep)


listStep : ( Int, List Package.Package ) -> D.Decoder (D.Step ( Int, List Package.Package ) (List Package.Package))
listStep ( n, pkgs ) =
  if n <= 0
  then D.succeed (D.Done pkgs)
  else D.map (\pkg -> D.Loop ( n - 1, pkg :: pkgs )) decodeOne


decodeListLength : D.Decoder Int
decodeListLength =
  -- Haskell gives 64 byte lengths for lists, but we don't
  -- need that much so we ignore the rest.
  D.map2 (\_ len -> len)
    (D.unsignedInt32 BE)
    (D.unsignedInt32 BE)


decodeOne : D.Decoder Package.Package
decodeOne =
  D.map3 Package.Package
    decodeSizedString
    decodeSizedString
    decodeVersion


decodeSizedString : D.Decoder String
decodeSizedString =
  D.unsignedInt8
    |> D.andThen D.string


decodeVersion : D.Decoder V.Version
decodeVersion =
  let decodeMinorAndPatch firstNumber =
        if firstNumber == 255 then
            D.map3 V.Version
              (D.unsignedInt16 BE)
              (D.unsignedInt16 BE)
              (D.unsignedInt16 BE)
          else
            D.map2 (V.Version firstNumber)
              D.unsignedInt8
              D.unsignedInt8
  in
  D.unsignedInt8
    |> D.andThen decodeMinorAndPatch
