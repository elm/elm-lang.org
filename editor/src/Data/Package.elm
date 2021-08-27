module Data.Package exposing (..)

import Dict exposing (Dict)
import Data.Version as Version exposing (Version(..))
import Json.Decode as D


defaults : List Package
defaults =
  [ Package "elm" "browser" "" (Version 1 0 1) 0
  , Package "elm" "core" "" (Version 1 0 0) 0
  , Package "elm" "html" "" (Version 1 0 0) 0
  , Package "elm" "json" "" (Version 1 1 3) 0
  , Package "elm" "url" "" (Version 1 0 0) 0
  , Package "elm" "virtual-dom" "" (Version 1 0 2) 0
  ]


toDict : List Package -> Dict String Package
toDict =
  let toPair i p = ( toName p, { p | order = i } ) in
  Dict.fromList << List.indexedMap toPair


merge : Dict String Package -> Dict String Package -> Dict String ( Package, Maybe Version )
merge installed all =
  let onlyInInstalled key package =
        Dict.insert key ( package, Just package.version )

      inBoth key ins package =
        Dict.insert key ( package, Just ins.version )

      onlyInAll key package =
        Dict.insert key ( package, Nothing )
  in
  Dict.merge onlyInInstalled inBoth onlyInAll installed all Dict.empty


type alias Package =
  { author : String
  , project : String
  , summary : String
  , version : Version
  , order : Int
  }


toName : Package -> String
toName p =
  p.author ++ "/" ++ p.project


isInstalled : Maybe Version -> Bool
isInstalled installed =
  case installed of
    Just _ -> True
    Nothing -> False


decoder : D.Decoder Package
decoder =
  let decodeAuthor name =
        case String.split "/" name of
          [ author, _ ] -> D.succeed author
          _             -> D.fail ("Could not decoder package name: " ++ name)

      decodeProject name =
        case String.split "/" name of
          [ _, project ]  -> D.succeed project
          _               -> D.fail ("Could not decoder package name: " ++ name)
  in
  D.map5 Package
    (D.field "name" <| D.andThen decodeAuthor D.string)
    (D.field "name" <| D.andThen decodeProject D.string)
    (D.field "summary" D.string)
    (D.field "version" Version.decoder)
    (D.succeed 1)


encodeAsString : List Package -> String
encodeAsString packages =
  let encodeOne package =
        toName package ++ ":" ++ Version.toString package.version
  in
  String.join "," (List.map encodeOne packages)


search : String -> List ( Package, Maybe Version ) -> List ( Package, Maybe Version )
search query packages =
  let queryTerms =
        String.words (String.toLower query)

      matchesAllTerms ( entry, _ ) =
        let
          lowerName =
            String.toLower (toName entry)

          lowerSummary =
            String.toLower entry.summary

          matchesTerm term =
            String.contains term lowerName
            || String.contains term lowerSummary
        in
        List.all matchesTerm queryTerms
  in
  List.filter matchesAllTerms packages