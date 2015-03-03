{--  Dict  -------------------------------------------------

A data structure that allows key-value pairs to be stored,
found, and combined efficiently.

This file defines the `add` and `groupBy` functions which
can be quite helpful for creating complex dictionaries.

-----------------------------------------------------------}

import Dict
import Graphics.Element exposing (..)


type alias Tree =
    { family : String
    , genus : String
    }


trees : List Tree
trees =
    [ Tree "Rosaceae" "Crataegus"
    , Tree "Theaceae" "Gordonia"
    , Tree "Ulmaceae" "Ulmus"
    , Tree "Rosaceae" "Pyrus"
    , Tree "Rosaceae" "Malus"
    , Tree "Theaceae" "Stewartia"
    , Tree "Rosaceae" "Prunus"
    , Tree "Ulmaceae" "Zelkova"
    ]


add : comparable -> value -> Dict.Dict comparable (List value) -> Dict.Dict comparable (List value)
add key value dict =
    let update currentValue =
          case currentValue of
            Nothing -> Just [value]
            Just vs -> Just (value :: vs)
    in
        Dict.update key update dict


groupBy : (a -> comparable) -> (a -> value) -> List a -> Dict.Dict comparable (List value)
groupBy getKey getValue values =
    List.foldl (\v d -> add (getKey v) (getValue v) d) Dict.empty values


familyToGenera : Dict.Dict String (List String)
familyToGenera =
    groupBy .family .genus trees


display : (String, a) -> Element
display (family, genera) =
    width 100 (plainText family) `beside` show genera


header : Element
header =
    plainText "Trees: Genera in each Family"


main : Element
main =
    flow down
        (header :: List.map display (Dict.toList familyToGenera))
