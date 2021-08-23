module Deps exposing
  ( Info
  , ModuleInfo
  , decoder
  )


import Dict exposing (Dict)
import Json.Decode as D
import Set exposing (Set)



-- DOCS


type alias Info =
  Dict String ModuleInfo


type alias ModuleInfo =
  { pkg : String
  , ops : Set String
  , values : Set String
  , aliases : Set String
  , unions : Dict String (List String)
  }



-- DECODER


decoder : D.Decoder Info
decoder =
  D.dict moduleInfoDecoder


moduleInfoDecoder : D.Decoder ModuleInfo
moduleInfoDecoder =
  D.map5 ModuleInfo
    (D.field "pkg" D.string)
    (D.field "ops" stringSet)
    (D.field "values" stringSet)
    (D.field "aliases" stringSet)
    (D.field "types" (D.dict (D.list D.string)))


stringSet : D.Decoder (Set String)
stringSet =
  D.map Set.fromList (D.list D.string)
