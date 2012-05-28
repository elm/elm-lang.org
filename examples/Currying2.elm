
  How you should think.
-------------------------

increment = \n -> n + 1

increment 4
  -->  (\n -> n + 1) 4
  -->  4 + 1
  -->  5

plus x = \y -> x + y

plus = \x -> \y -> x + y


plus 3 4
  -->  (\x -> \y -> x + y) 3 4
  -->  (\y -> 3 + y) 4
  -->  3 + 4
  -->  7

plus 1
  -->  (\x -> \y -> x + y) 1
  -->  (\y -> 1 + y)