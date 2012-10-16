
import Website.Docs (createDocs)

basics =
  [ ("empty", "Map k v", "Create an empty map.")
  , ("singleton", "k -> v -> Map k v", "Create a map with one key-value pair.")
  , ("insert", "k -> v -> Map k v -> Map k v", "Insert a key-value pair into a map. Replaces value when there is a collision.")
  , ("lookup", "k -> Map k v -> Maybe v", "Lookup the value associated with a key.")
  , ("remove", "k -> Map k v -> Map k v", "Remove a key-value pair from a map. If the key is not found, no changes are made.")
  , ("member", "k -> Map k v -> Bool", "Determine if a key is in a map.")
--  , ("keys", "Map k v -> [k]", "Get all of the keys in a map.")
--  , ("values", "Map k v -> [v]", "Get all of the values in a map.")
  ]

categories = [ ("Using Dictionaries", basics) ]

main = createDocs "Dict" categories
