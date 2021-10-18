module Data.Version exposing
  ( Version(..)
  , toString, fromString
  , decoder, encode
  )

import Json.Decode as D
import Json.Encode as E


type Version
  = Version Int Int Int


toString : Version -> String
toString (Version ma mi pa) =
  String.fromInt ma ++ "." ++ String.fromInt mi ++ "." ++ String.fromInt pa


fromString : String -> Maybe Version
fromString string =
  case String.split "." string of
    [ maS, miS, paS ] ->
      case ( String.toInt maS, String.toInt miS, String.toInt paS ) of
        ( Just ma, Just mi, Just pa ) ->
          Just (Version ma mi pa)

        _ ->
          Nothing

    _ ->
      Nothing


decoder : D.Decoder Version
decoder =
  let validate s =
        case fromString s of
          Just version ->
            D.succeed version

          Nothing ->
            D.fail "Version was of wrong format."
  in
  D.andThen validate D.string


encode : Version -> E.Value
encode =
  E.string << toString