
import Website.Docs (createDocs)

def =
  [ ("data Maybe a = Just a | Nothing", "", "The Maybe datatype. Useful when a computation may or may not result in a value (e.g. logarithm is defined only for positive numbers).")
  ]

basics =
  [ ("maybe", "b -> (a -> b) -> Maybe a -> b", "Apply a function to the contents of a Maybe. Return default when given Nothing.")
  , ("isJust", "Maybe a -> Bool", "Check if constructed with Just.")
  , ("isNothing", "Maybe a -> Bool", "Check if constructed with Nothing.")
  , ("fromMaybe", "a -> Maybe a -> a", "Extract the value of from a Maybe. Returns default when given Nothing.")
  ]

lists =
  [ ("consMaybe", "Maybe a -> [a] -> [a]", "If Just, adds the value to the front of the list. If Nothing, list is unchanged.")
  , ("catMaybes", "[Maybe a] -> [a]", "Filters out Nothings and extracts the remaining values.")
  , ("mapMaybe", "(a -> Maybe b) -> [a] -> [b]", "Maps function onto a list, discarding all Nothing results from the resulting list.")
  ]

categories = [ ("Definition", def), ("Basic Utilities", basics), ("Maybe with Lists", lists) ]

main = createDocs "Maybe" categories
