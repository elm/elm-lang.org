module Data.PackageList exposing (..)

import Dict exposing (Dict)
import Data.Version as Version exposing (Version(..))
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Json.Encode as E
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
  | Failed


-- MANY / INIT


preinstalled : Packages
preinstalled =
  let toPair pkg =
        ( toKey pkg, ( pkg, Installed pkg.version ) )
  in
  Dict.fromList (List.map toPair defaults)


defaults : List Package
defaults =
  [ Package "elm" "browser" (Version 1 0 2) 0
  , Package "elm" "core" (Version 1 0 5) 0
  , Package "elm" "html" (Version 1 0 0) 0
  , Package "elm" "json" (Version 1 1 3) 0
  , Package "elm" "url" (Version 1 0 0) 0
  , Package "elm" "virtual-dom" (Version 1 0 2) 0
  ]


fetch : (Result Http.Error (List Package) -> msg) -> Cmd msg
fetch onResult =
  Http.get
    { url = "http://localhost:8000/compile/packages/all" -- TODO
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
        ( toKey pkg, { pkg | order = index } )
  in
  Dict.fromList (List.indexedMap toPair packages)



-- MANY / INSTALLATION


attemptInstall : (Package -> Result Http.Error String -> msg) -> Package -> Cmd msg
attemptInstall onResult package =
  let payload =
        E.object
          [ ( "author", E.string package.author )
          , ( "project", E.string package.project )
          ]
  in
  Http.riskyRequest
    { method = "POST"
    , headers = []
    , url = "http://localhost:8000/compile/packages/install" -- TODO
    , body = Http.jsonBody payload
    , expect = Http.expectString (onResult package)
    , timeout = Nothing
    , tracker = Nothing
    }


setInstallation : Package -> Installation -> Packages -> Packages
setInstallation pkg installation =
  Dict.insert (toKey pkg) ( pkg, installation )


getInstalled : Packages -> List ( Package, Installation )
getInstalled packages =
  let keepInstalled ( pkg, installedVersion ) =
        case installedVersion of
          NotInstalled  -> False
          Installing    -> True
          Installed _   -> True
          Incompatible  -> True
          Failed        -> True
  in
  packages
    |> Dict.values
    |> List.filter keepInstalled
    |> List.sortBy (Tuple.first >> .order)



-- MANY / SEARCH


fromQuery : String -> Packages -> List ( Package, Installation )
fromQuery query packages =
  let prioritizeInstalled ( package, installation ) =
        case installation of
          NotInstalled  -> ( 0, package.order )
          Installing    -> ( 1, package.order )
          Installed _   -> ( 1, package.order )
          Incompatible  -> ( 1, package.order )
          Failed        -> ( 1, package.order )
  in
  packages
    |> Dict.values
    |> List.sortBy prioritizeInstalled
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



-- SINGLE


type alias Package =
  { author : String
  , project : String
  , version : Version
  , order : Int
  }


toKey : Package -> ( String, String )
toKey pkg =
  ( pkg.author, pkg.project )


toName : Package -> String
toName pkg =
  pkg.author ++ "/" ++ pkg.project



-- DECODER


decoder : D.Decoder (List Package)
decoder =
  decodeListLength
    |> D.andThen (\len -> D.loop ( len, [] ) listStep)


listStep : ( Int, List Package ) -> D.Decoder (D.Step ( Int, List Package ) (List Package))
listStep ( n, pkgs ) =
  if n <= 0
  then D.succeed (D.Done pkgs)
  else D.map (\pkg -> D.Loop ( n - 1, pkg n :: pkgs )) decodeOne


decodeListLength : D.Decoder Int
decodeListLength =
  -- Haskell gives 64 byte lengths for lists, but we don't
  -- need that much so we ignore the rest.
  D.map2 (\_ len -> len)
    (D.unsignedInt32 BE)
    (D.unsignedInt32 BE)


decodeOne : D.Decoder (Int -> Package)
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
