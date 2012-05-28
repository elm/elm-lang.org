map f xs = case xs of { x:xs -> f x : map f xs ; [] -> [] }
intersperse sep xs =
  case xs of { a:b:cs -> a : sep : intersperse sep (b:cs)
             ; a:[] -> [a] ; [] -> [] }
intercalate sep xs =
  case xs of { a:b:cs -> a ++ sep ++ intercalate sep (b:cs)
             ; a:[] -> a ; [] -> [] }

----------------------

button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (245/255) (245/255) (245/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , width outer . box 2 $ plainText "&nbsp;" `above` body inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces px = map (\f -> f ()) . intersperse (\x -> height px $ plainText "") . map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

darkerGrey = rgb (114/255) (124/255) (129/255)
entry w (name, type, desc) =
  flow down . map (width w) $
    [ text . monospace . toText $ name ++ " :: " ++ type
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . map (width w) $ (text . bold $ toText name) : map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : map (group w) cats in
  lift (skeleton f) Window.width

----------------------

basics =
  [ ("(:)", "a -> [a] -> [a]", "Add an element to the front of a list. Thus, (a : [b,c] = [a,b,c])")
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
  , ("zipWith", "(a -> b -> c) -> [a] -> [b] -> [c]", "Combine two lists, combining them with the given function. If one input list has extra elements (it is longer), those elements are dropped. Note: zip = zipWith (,).")
  ]

categories =
  [ ("Basic Utilities", basics)
  , ("List Transformations", transforms)
  , ("Folds (Reducing Lists)", folds)
  , ("Special Folds", special)
  , ("Other Useful Functions", other) ]

main = createDocs "List" categories
