
{----------------------------------------------------------------

Overview:
  filter takes two arguments: a predicate and a list.

  A predicate is a function from anything to Bool (a -> Bool).
  A value v 'satisfies' the predicate pred if (pred v) evaluates
  to True.

  filter returns a new list, keeping only the list elements
  that satisfy the predicate.

----------------------------------------------------------------}


main = asText <| filter isEven [1..10]

isEven n = n % 2 == 0