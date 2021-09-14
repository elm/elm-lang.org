module Data.PackageList exposing (..)

import Dict exposing (Dict)
import Data.Version as Version exposing (Version(..))
import Bytes exposing (Endianness(..))
import Bytes.Decode as D


type alias Package =
  { author : String
  , project : String
  , version : Version
  , order : Int
  }


defaults : List Package
defaults =
  [ Package "elm" "browser" (Version 1 0 2) 0
  , Package "elm" "core" (Version 1 0 5) 0
  , Package "elm" "html" (Version 1 0 0) 0
  , Package "elm" "json" (Version 1 1 3) 0
  , Package "elm" "url" (Version 1 0 0) 0
  , Package "elm" "virtual-dom" (Version 1 0 2) 0
  ]



-- DECODER


decode : D.Decoder (List Package)
decode =
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
