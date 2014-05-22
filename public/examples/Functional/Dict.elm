{--  Dict  -------------------------------------------------

A data structure that allows key-value pairs to be stored,
found, and combined efficiently.

This file defines the `add` and `groupBy` functions which
can be quite helpful for creating complex dictionaries.

-----------------------------------------------------------}

import Dict

trees = [{ family="Rosaceae", genus="Crataegus" },
         { family="Theaceae", genus="Gordonia"  },
         { family="Ulmaceae", genus="Ulmus"     },
         { family="Rosaceae", genus="Pyrus"     },
         { family="Rosaceae", genus="Malus"     },
         { family="Theaceae", genus="Stewartia" },
         { family="Rosaceae", genus="Prunus"    },
         { family="Ulmaceae", genus="Zelkova"   }]

add k v dict =
    let vs = Dict.getOrElse [] k dict
    in  Dict.insert k (v::vs) dict

groupBy f g vs =
    foldl (\v d -> add (f v) (g v) d) Dict.empty vs

familyToGenera =
    groupBy .family .genus trees

display (family,genera) =
    width 100 (plainText family) `beside` asText genera

header = [markdown|## Trees: Genera in each Family|]

main = flow down
       (header :: map display (Dict.toList familyToGenera))
