import Html exposing (text)


main =
  text (toString (length (List.range 1 9)))


{-| Figure out the length of any list. To find the length of
list [6,6,6] we need to know that this devilish list is just
a convenient way to write (6 :: (6 :: (6 :: []))) where
the :: operator is putting an element on the front of a list.
Evaluation looks like this:

    length (6 :: (6 :: (6 :: [])))
    1 + (length (6 :: (6 :: [])))
    1 + (1 + (length (6 :: [])))
    1 + (1 + (1 + length []))
    1 + (1 + (1 + 0))
    1 + (1 + 1)
    1 + 2
    3

Stepping through evaluation like this can be really helpful
for building intuition about recursive functions.
-}
length : List a -> Int
length list =
  case list of
    [] ->
        0

    first :: rest ->
        1 + length rest


{- EXPLANATION

The 'length' function figures out the length of any list. We
use the 'case' keyword to "pattern match" on the structure of
the list.

Lists can either be empty [] or a pair of an element and a
sublist like (1 :: []) or (1 :: (2 :: []))

To write 'length' we have two cases to think about:

  1. If the list is empty, the length is zero.
  2. If the list is not empty, the length is 1 more than
     however long the rest of the list is.

When you write a function like 'length', always pretend that
you already succeeded. So when we need to know how long the
rest of the list is, we can pretend 'length' is already done
and use it!
-}
