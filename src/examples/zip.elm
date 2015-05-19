import Graphics.Element exposing (show)


-- Zip two lists together. In this case, we are pairing up
-- names and ages.
main =
  show (zip ["Tom", "Sue", "Bob"] [45, 31, 26])


{-| The zip function takes in two lists and returns a combined
list. It combines the elements of each list pairwise until one
of the lists runs out of elements.

    zip [1,2,3] ['a','b','c'] == [(1,'a'), (2,'b'), (3,'c')]

-}
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'

    (_, _) ->
        []


-- There is a function in the List library called map2 that
-- applies a function pairwise to two lits. You can use it
-- to define 'zip' much more easily:
--
--     zip = List.map2 (,)
--
-- The (,) expression is a shortcut to create 2-tuples, so
-- evaluating ((,) 3 4) results in (3,4)