
{----------------------------------------------------------------

Overview:
  zip is a way of combining related data from two lists into one.
  The elements of each list are combined pairwise into tuples.

  When the input lists are of different lengths, the resulting
  list is as long as the shortest input list.

  In this example, we are pairing names and ages.

----------------------------------------------------------------}


zip listX listY =
  case (listX, listY) of
    (x:xs, y:ys) -> (x,y) : zip xs ys
    _ -> []

main = asText (zip ["Tom", "Sue", "Bob"] [45, 31, 26])