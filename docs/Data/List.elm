
import Website.Docs (createDocs)

basics =
  [ ("(:)", "a -> [a] -> [a]", "Add an element to the front of a list (a : [b,c] = [a,b,c]).")
  , ("(++)", "[a] -> [a] -> [a]", "Appends two lists.")
  , ("head", "[a] -> a", "Extract the first element of a list. List must be non-empty.")
  , ("tail", "[a] -> [a]", "Extract the elements after the head of the list. List must be non-empty.")
  , ("length", "[a] -> Int", "Determine the length of a list.")
  ]

transforms =
  [ ("map", "(a -> b) -> [a] -> [b]", "Apply a function to every element of a list.")
  , ("reverse", "[a] -> [a]", "Reverse a list.")
  , ("intersperse", "a -> [a] -> [a]", "Places the given value between all members of the given list.")
  , ("intercalate", "[a] -> [[a]] -> [a]", "Places the given value between all of the lists in the second argument and concatenates the result. Note: intercalate xs xss = concat (intersperse xs xss)")
  ]

folds =
  [ ("foldr", "(a -> b -> b) -> b -> [a] -> b", "Reduce a list from the right.")
  , ("foldl", "(a -> b -> b) -> b -> [a] -> b", "Reduce a list from the left.")
  , ("foldr1", "(a -> a -> a) -> [a] -> a", "Reduce a list from the right without a base case. List must be non-empty.")
  , ("foldl1", "(a -> a -> a) -> [a] -> a", "Reduce a list from the left without a base case. List must be non-empty.")
  , ("scanl", "(a -> b -> b) -> b -> [a] -> [b]", "Reduce a list from the left, building up all of the intermediate results into a list.")
  , ("scanl1", "(a -> a -> a) -> [a] -> [a]", "Same as scanl but it doesn't require a base case. List must be non-empty.")
  ]

special =
  [ ("concat", "[[a]] -> [a]", "Flatten a list of lists.")
  , ("concatMap", "(a -> [b]) -> [a] -> [b]", "Map a given function onto a list and flatten the resulting lists. (concatMap f xs == concat (map f xs))")
  , ("and", "[Bool] -> Bool", "Check to see if all elements are True.")
  , ("or", "[Bool] -> Bool", "Check to see if any elements are True.")
  , ("forall", "(a -> Bool) -> [a] -> Bool", "Check to see if all elements satisfy the predicate.")
  , ("exists", "(a -> Bool) -> [a] -> Bool", "Check to see if any elements satisfy the predicate.")
  , ("sum", "[Int] -> Int", "Get the sum of the list elements.")
  , ("product", "[Int] -> Int", "Get the product of the list elements.")
  , ("maximum", "[Int] -> Int", "Find the highest number in a non-empty list.")
  , ("minimum", "[Int] -> Int", "Find the lowest number in a non-empty list.")
  ]

other =
  [ ("filter", "(a -> Bool) -> [a] -> [a]", "Filter out elements which do not satisfy the predicate.")
  , ("sort", "[Number] -> [Number]", "Sorts a list of numbers.")
  , ("partition", "(a -> Bool) -> [a] -> ([a],[a])", "Split a list based on the predicate.")
  , ("zip", "[a] -> [b] -> [(a,b)]", "Combine two lists, combining them into tuples pairwise. If one input list has extra elements (it is longer), those elements are dropped.")
  , ("zipWith", "(a -> b -> c) -> [a] -> [b] -> [c]", "Combine two lists, combining them with the given function. If one input list has extra elements (it is longer), those elements are dropped.")
  , ("take", "Int -> [a] -> [a]", toText "Take the first n members of a list. Thus, " ++ monospace (toText "take 2 [1,2,3,4] ==> [1,2]"))
  , ("drop", "Int -> [a] -> [a]", toText "Drop the first n members of a list. Thus, " ++ monospace (toText "drop 2 [1,2,3,4] ==> [3,4]"))
  ]

categories =
  [ ("Basic Utilities", basics)
  , ("List Transformations", transforms)
  , ("Folds (Reducing Lists)", folds)
  , ("Special Folds", special)
  , ("Other Useful Functions", other) ]

main = createDocs "Data.List" categories
