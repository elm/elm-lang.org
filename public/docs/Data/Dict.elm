
import Website.Docs (createDocs)

build =
  [ ("empty", "Dict k v", "Create an empty dictionary.")
  , ("singleton", "k -> v -> Dict k v", "Create a dictionary with one key-value pair.")
  , ("insert", "k -> v -> Dict k v -> Dict k v", "Insert a key-value pair into a dictionary. Replaces value when there is a collision.")
  , ("remove", "k -> Dict k v -> Dict k v", "Remove a key-value pair from a dictionary. If the key is not found, no changes are made.")
  ]

query =
  [ ("member", "k -> Dict k v -> Bool", "Determine if a key is in a dictionary.")
  , ("lookup", "k -> Dict k v -> Maybe v", "Lookup the value associated with a key.")
  , ("findWithDefault", "v -> k -> Dict k v -> v", "Find the value associated with a key. If the key is not found, return the default value.")
--  , ("find", "k -> Dict k v -> v", "Find the value associated with a key. If the key is not found, there will be a runtime error.")
  ]

combine =
  [ ("union", "Dict k v -> Dict k v -> Dict k v", "Combine two dictionaries. If there is a collision, preference is given to the first dictionary.")
  , ("intersect", "Dict k v -> Dict k w -> Dict k v", "Keep a key-value pair when its key appears in the second dictionary. Preference is given to values in the first dictionary.")
  , ("diff", "Dict k v -> Dict k w -> Dict k v", "Keep a key-value pair when its key does not appear in the second dictionary. Preference is given to the first dictionary.")
  ]

morph =
  [ ("map", "(a -> b) -> Dict k a -> Dict k b"
    , "Apply a function to all values in a dictionary.")
  , ("foldl", "(k -> v -> b -> b) -> b -> Dict k v -> b"
    , "Fold over the key-value pairs in a dictionary, in order from lowest key to highest key.")
  , ("foldr", "(k -> v -> b -> b) -> b -> Dict k v -> b"
    , "Fold over the key-value pairs in a dictionary, in order from highest key to lowest key.")
  ]

lists =
  [ ("keys", "Dict k v -> [k]", "Get all of the keys in a dictionary.")
  , ("values", "Dict k v -> [v]", "Get all of the values in a dictionary.")
  , ("toList", "Dict k v -> [(k,v)]", "Convert a dictionary into an association list of key-value pairs.")
  , ("fromList", "[(k,v)] -> Dict k v", "Convert an association list into a dictionary.")
  ]

categories = [ ("Build", build)
             , ("Query", query)
             , ("Combine", combine)
             , ("Morph", morph)
             , ("Lists", lists)
             ]

main = createDocs "Dict" categories
