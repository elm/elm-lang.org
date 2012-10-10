
import Website.Docs (createDocs)

basics =
  [ ("empty", "Set a", "Create an empty set.")
  , ("singleton", "a -> Set a", "Create a set with one value.")
  , ("insert", "a -> Set a -> Set a", "Insert a value into a set.")
  , ("remove", "a -> Set a -> Set a", "Remove a value from a set. If the value is not found, no changes are made.")
  , ("member", "a -> Set a -> Bool", "Determine if a value is in a set.")
  ]

categories = [ ("Using Sets", basics) ]

main = createDocs "Data.Set" categories
