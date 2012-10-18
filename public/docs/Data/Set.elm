
import Website.Docs (createDocs)

build =
  [ ("empty", "Set a", "Create an empty set.")
  , ("singleton", "a -> Set a", "Create a set with one value.")
  , ("insert", "a -> Set a -> Set a", "Insert a value into a set.")
  , ("remove", "a -> Set a -> Set a", "Remove a value from a set. If the value is not found, no changes are made.")
  ]

query =
  [ ("member", "a -> Set a -> Bool", "Determine if a value is in a set.")
  ]

combine =
  [ ("union", "Set a -> Set a -> Set a", "Get the union of two sets. Keep all values.")
  , ("intersect", "Set a -> Set a -> Set a", "Get the intersection of two sets. Keeps values that appear in both sets.")
  , ("diff", "Set a -> Set a -> Set a", "Get the difference between the first set and the second. Keeps values that do not appear in the second set.")
  ]

morph =
  [ ("fold", "(a -> b -> b) -> b -> Set a -> b"
    , "Fold over the values in a set.")
  , ("map", "(a -> b) -> Set a -> Set b"
    , "Map a function onto a set, creating a new set with no duplicates.")
  ]

lists =
  [ ("toList", "Set a -> [a]", "Convert a set into a list.")
  , ("fromList", "[a] -> Set a", "Convert a list into a set, removing any duplicates.")
  ]

categories = [ ("Build", build)
             , ("Query", query)
             , ("Combine", combine)
             , ("Lists", lists)
             , ("Morph", morph)
             ]

main = createDocs "Set" categories
