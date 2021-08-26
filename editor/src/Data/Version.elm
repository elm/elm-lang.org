module Data.Version exposing (..)


type Version
  = Version Int Int Int


toString : Version -> String
toString (Version ma mi pa) =
  String.fromInt ma ++ "." ++ String.fromInt mi ++ "." ++ String.fromInt pa