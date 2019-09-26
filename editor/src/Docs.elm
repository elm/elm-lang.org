module Docs exposing (Docs, ModuleInfo, decoder)


import Dict exposing (Dict)
import Json.Decode as D
import Set exposing (Set)



-- DOCS


type alias Docs =
  Dict String ModuleInfo


type alias ModuleInfo =
  { home : String -- something like "elm/core/1.0.2"
  , values : Set String -- includes operators
  , types : Set String -- includes aliases and custom types
  , variants : Dict String String -- only includes variant names
  }



-- DECODER


decoder : D.Decoder Docs
decoder =
  D.dict moduleInfoDecoder



-- MODULE INFO DECODER


moduleInfoDecoder : D.Decoder ModuleInfo
moduleInfoDecoder =
  D.map4 toModuleInfo
    (D.field "home" D.string)
    (D.field "values" (D.list D.string))
    (D.field "aliases" (D.list D.string))
    (D.field "types" (D.dict (D.list D.string)))


toModuleInfo : String -> List String -> List String -> Dict String (List String) -> ModuleInfo
toModuleInfo home values aliases types =
  { home = home
  , values = Set.fromList values
  , types = Set.fromList (aliases ++ Dict.keys types)
  , variants = Dict.fromList (Dict.foldr addVariants [] types)
  }


addVariants : String -> List String -> List (String, String) -> List (String, String)
addVariants typeName variants pairs =
  List.foldl (\variant ps -> (variant, typeName) :: ps) pairs variants



