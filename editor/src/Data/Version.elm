module Data.Version exposing (..)


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