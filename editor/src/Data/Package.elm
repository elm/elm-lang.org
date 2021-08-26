module Data.Package exposing (..)

import Dict exposing (Dict)
import Data.Version as Version exposing (Version(..))
import Json.Decode as D


defaults : Dict String Version
defaults =
  Dict.fromList
    [ ( "elm/browser", Version 1 0 1 )
    , ( "elm/core", Version 1 0 2 )
    , ( "elm/html", Version 1 0 0 )
    , ( "elm/json", Version 1 1 3 )
    , ( "elm/time", Version 1 0 0 )
    , ( "elm/url", Version 1 0 0 )
    , ( "elm/virtual-dom", Version 1 0 2 )
    ]


type alias Package =
  { name : String
  , author : String
  , project : String
  , summary : String
  , version : Version
  }


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
    (D.field "name" D.string)
    (D.field "name" <| D.andThen decodeAuthor D.string)
    (D.field "name" <| D.andThen decodeProject D.string)
    (D.field "summary" D.string)
    (D.field "version" Version.decoder)


search : String -> List Package -> List Package
search query packages =
  let queryTerms =
        String.words (String.toLower query)

      matchesAllTerms entry =
        let
          lowerName =
            String.toLower entry.name

          lowerSummary =
            String.toLower entry.summary

          matchesTerm term =
            String.contains term lowerName
            || String.contains term lowerSummary
        in
        List.all matchesTerm queryTerms
  in
  List.filter matchesAllTerms packages