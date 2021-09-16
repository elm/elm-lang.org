module Data.PackageList exposing (..)

import Dict exposing (Dict)
import Data.Version as Version exposing (Version(..))
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Json.Encode as JE
import Json.Decode as JD
import Http


-- MANY


type alias Packages =
  Dict ( String, String ) ( Package, Installation )


type Installation
  = NotInstalled
  | Installing
  | Installed Version
  --| Settled Version
  | Incompatible
  | Failed String (Maybe String)


-- MANY / INIT


preinstalled : Packages
preinstalled =
  let toPair pkg =
        ( toKey pkg, ( pkg, Installed pkg.version ) )
  in
  Dict.fromList (List.map toPair defaults)


defaults : List Package
defaults =
  [ Package "elm" "browser" (Version 1 0 2)
  , Package "elm" "core" (Version 1 0 5)
  , Package "elm" "html" (Version 1 0 0)
  , Package "elm" "json" (Version 1 1 3)
  , Package "elm" "url" (Version 1 0 0)
  , Package "elm" "virtual-dom" (Version 1 0 2)
  ]


fetch : (Result Http.Error (List Package) -> msg) -> Cmd msg
fetch onResult =
  Http.get
    { url = "http://localhost:8000/packages/all" -- TODO
    , expect = Http.expectBytes onResult decoder
    }



-- MANY / UPDATE


fromNews : List Package -> Packages
fromNews news =
  let onlyInInstalled key installed =
        Dict.insert key ( installed, Installed installed.version )

      inBoth key installed package =
        Dict.insert key ( package, Installed installed.version )

      onlyInNews key package =
        Dict.insert key ( package, NotInstalled )
  in
  Dict.merge onlyInInstalled inBoth onlyInNews (fromList defaults) (fromList news) Dict.empty


fromList : List Package -> Dict ( String, String ) Package
fromList packages =
  let toPair index pkg =
        ( toKey pkg, pkg )
  in
  Dict.fromList (List.indexedMap toPair packages)



-- MANY / INSTALLATION


attemptInstall : (Package -> Result Http.Error Installation -> msg) -> Package -> Cmd msg
attemptInstall onResult package =
  let payload =
        JE.object
          [ ( "author", JE.string package.author )
          , ( "project", JE.string package.project )
          ]
  in
  Http.riskyRequest
    { method = "POST"
    , headers = []
    , url = "http://localhost:8000/packages/install" -- TODO
    , body = Http.jsonBody payload
    , expect = Http.expectJson (onResult package) (decodeInstallResult package)
    , timeout = Nothing
    , tracker = Nothing
    }


decodeInstallResult : Package -> JD.Decoder Installation
decodeInstallResult package =
  let decodeStatus =
        JD.string |> JD.andThen (\s ->
            case s of
              "Success"                 -> JD.succeed (Installed package.version)
              "InstallNoOutline"        -> errorWithoutPackage "InstallNoOutline"
              "InstallBadOutline"       -> errorWithoutPackage "InstallBadOutline"
              "InstallBadRegistry"      -> errorWithoutPackage "InstallBadRegistry"
              "InstallNoArgs"           -> errorWithoutPackage "InstallNoArgs"
              "InstallHadSolverTrouble" -> errorWithoutPackage "InstallHadSolverTrouble"
              "InstallBadDetails"       -> errorWithoutPackage "InstallBadDetails"
              "InstallNoOnlineAppSolution"    -> errorWithPackage "InstallNoOnlineAppSolution"
              "InstallNoOfflineAppSolution"   -> errorWithPackage "InstallNoOfflineAppSolution"
              "InstallNoOnlinePkgSolution"    -> errorWithPackage "InstallNoOnlinePkgSolution"
              "InstallNoOfflinePkgSolution"   -> errorWithPackage "InstallNoOfflinePkgSolution"
              "InstallUnknownPackageOnline"   -> errorWithPackage "InstallUnknownPackageOnline"
              "InstallUnknownPackageOffline"  -> errorWithPackage "InstallUnknownPackageOffline"
              _                               -> errorWithoutPackage "UnknownStatus"
          )

      errorWithoutPackage msg =
        JD.succeed (Failed msg Nothing)

      errorWithPackage msg =
        JD.map (Failed msg << Just) (JD.field "package" JD.string)
  in
  JD.field "status" decodeStatus


setInstallation : Package -> Installation -> Packages -> Packages
setInstallation pkg installation =
  Dict.insert (toKey pkg) ( pkg, installation )


getInstalled : Packages -> List ( Package, Installation )
getInstalled packages =
  let keepInstalled ( pkg, installedVersion ) =
        isInstalled installedVersion
  in
  packages
    |> Dict.values
    |> List.filter keepInstalled


getAll : Packages -> List ( Package, Installation )
getAll packages =
  packages
    |> Dict.values


isInstalled : Installation -> Bool
isInstalled installation =
  case installation of
    NotInstalled  -> False
    Installing    -> True
    Installed _   -> True
    Incompatible  -> True
    Failed _ _    -> True



-- MANY / SEARCH


fromQuery : String -> Packages -> List ( Package, Installation )
fromQuery query packages =
  let onlyNotInstalled ( package, installation ) =
        not (isInstalled installation)
  in
  packages
    |> Dict.values
    |> List.filter onlyNotInstalled
    |> search query


search : String -> List ( Package, Installation ) -> List ( Package, Installation )
search query packages =
  let queryTerms =
        String.words (String.toLower query)

      matchesAllTerms ( entry, _ ) =
        let
          lowerName =
            String.toLower (toName entry)

          matchesTerm term =
            String.contains term lowerName
        in
        List.all matchesTerm queryTerms
  in
  List.filter matchesAllTerms packages



-- POPULAR


getPopular : Packages -> List ( Package, Installation )
getPopular packages =
  let getDictValue key =
        case Dict.get key packages of
          Just ( pkg, ins ) ->
            if isInstalled ins then Nothing else Just ( pkg, ins )

          Nothing ->
            Nothing
  in
  List.filterMap getDictValue popular


popular : List ( String, String )
popular =
  [ ( "elm", "http" )
  , ( "elm", "random" )
  , ( "elm", "time" )
  , ( "elm", "file" )
  , ( "elm", "json" )
  , ( "elm", "svg" )
  , ( "evancz", "elm-playground" )
  , ( "elm-explorations", "webgl" )
  , ( "w0rm", "elm-physics")
  , ( "rtfeldman", "elm-css" )
  , ( "mdgriffith", "elm-ui" )
  ]



-- SINGLE


type alias Package =
  { author : String
  , project : String
  , version : Version
  }


toKey : Package -> ( String, String )
toKey pkg =
  ( pkg.author, pkg.project )


toName : Package -> String
toName pkg =
  pkg.author ++ "/" ++ pkg.project


toDocsLink : Package -> String
toDocsLink package =
  "https://package.elm-lang.org/packages/" ++ toName package ++ "/" ++ Version.toString package.version



-- DECODER / ALL PACKAGES


decoder : D.Decoder (List Package)
decoder =
  decodeListLength
    |> D.andThen (\len -> D.loop ( len, [] ) listStep)


listStep : ( Int, List Package ) -> D.Decoder (D.Step ( Int, List Package ) (List Package))
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


decodeOne : D.Decoder Package
decodeOne =
  D.map3 Package
    decodeSizedString
    decodeSizedString
    decodeVersion


decodeSizedString : D.Decoder String
decodeSizedString =
  D.unsignedInt8
    |> D.andThen D.string


decodeVersion : D.Decoder Version
decodeVersion =
  let decodeMinorAndPatch firstNumber =
        if firstNumber == 255 then
            D.map3 Version
              (D.unsignedInt16 BE)
              (D.unsignedInt16 BE)
              (D.unsignedInt16 BE)
          else
            D.map2 (Version firstNumber)
              D.unsignedInt8
              D.unsignedInt8
  in
  D.unsignedInt8
    |> D.andThen decodeMinorAndPatch
