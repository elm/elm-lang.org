
{----------------------------------------------------------------

Overview:
  zip is a way of combining related data from two lists into one.
  The elements of each list are combined pairwise into tuples.

  When the input lists are of different lengths, the resulting
  list is as long as the shortest input list.

  In this example, we are pairing names and ages.

----------------------------------------------------------------}

import Graphics.Element exposing (show)


zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []


main =
  show (zip ["Tom", "Sue", "Bob"] [45, 31, 26])