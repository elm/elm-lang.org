{----------------------------------------------------------------

Overview:
  foldl and foldr are two types of folds. They are read "fold
  from the left" and "fold from the right". Each function takes
  three arguments:
    - combining function
    - base case / accumulator
    - list

  We can think of a fold as calculating some summary statistic
  for all of the elements in a list. The combining function takes
  an element from the list and the accumulated result so far, and
  puts them together. In foldl we traverse the list starting on
  the left. foldr starts on the right. Each element is combined
  with the accumulator until the whole list has been traversed.

  In this case, our combining function is addition. That means
  we will combine each element of the list with the accumulator
  by adding them together. This will give us the sum of the list.

  The accumulator starts at 0, the sum of an empty list.

----------------------------------------------------------------}


main : Element
main = asText <| foldl (+) 0 [1..5]
