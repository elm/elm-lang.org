import Html exposing (text)


main =
  text (toString (mergesort [5,3,8,1,9,4,7]))


{-| Sorts a list of values by:

  1. If the list has zero or one element, it is sorted!
  2. If it is longer, split it into two sublists
  3. Sort those sublists
  4. Merge these sorted sublists together

-}
mergesort : List comparable -> List comparable
mergesort list =
  case list of
    [] ->
        list

    [_] ->
        list

    _ ->
        let
          (xs, ys) = split list
        in
          merge (mergesort xs) (mergesort ys)


{-| Split a list into two sublists of nearly equal length.

    split [1,2,3,4,5,6] == ([2,4,6], [1,3,5])

-}
split : List a -> (List a, List a)
split list =
  case list of
    [] ->
        ([], [])

    x :: rest ->
        let
          (xs, ys) = split rest
        in
          (ys, x :: xs)


{-| Given two sorted lists, merge them together into a single
sorted list.

    merge [1,4,9] [2,3,5] == [1,2,3,4,5,9]

-}
merge : List comparable -> List comparable -> List comparable
merge xs ys =
  case (xs, ys) of
    (x :: xBack, y :: yBack) ->
        if x < y then
          x :: merge xBack ys
        else
          y :: merge xs yBack

    ([], rest) ->
        rest

    (rest, []) ->
        rest

