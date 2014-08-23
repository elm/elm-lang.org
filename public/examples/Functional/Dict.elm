{--  Dict  -------------------------------------------------

A data structure that allows key-value pairs to be stored,
found, and combined efficiently.

This file defines the `add` and `groupBy` functions which
can be quite helpful for creating complex dictionaries.

-----------------------------------------------------------}

import Dict

type Tree =
    { family : String
    , genus : String
    }

trees : [Tree]
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

add : comparable -> a -> Dict.Dict comparable [a] -> Dict.Dict comparable [a]
add key value dict =
    let update currentValue =
            case currentValue of
                Nothing -> Just [value]
                Just vs -> Just (value :: vs)
    in
        Dict.update key update dict

groupBy getKey getValue values =
    foldl (\v d -> add (getKey v) (getValue v) d) Dict.empty values

familyToGenera : Dict.Dict String [String]
familyToGenera =
    groupBy .family .genus trees

display : (String, a) -> Element
display (family, genera) =
    width 100 (plainText family) `beside` asText genera

header : Element
header = [markdown|## Trees: Genera in each Family|]

main : Element
main =
    flow down
        (header :: map display (Dict.toList familyToGenera))
