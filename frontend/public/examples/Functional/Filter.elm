
{----------------------------------------------------------------

Overview:
  filter takes two arguments: a predicate and a list.

      filter : (a -> Bool) -> List a -> List a

  filter returns a new list, keeping only the list elements
  that satisfy the predicate.

----------------------------------------------------------------}

import Graphics.Element exposing (show)


main =
  show (List.filter isEven [1..10])


isEven : Int -> Bool
isEven n =
  n % 2 == 0