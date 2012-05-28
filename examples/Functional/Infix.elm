
{----------------------------------------------------------------

Overview:
  Sometimes it is useful to use an infix operator such as
  addition (+) or cons (:) as a function.

  In Elm, when an infix operator is surrounded by parenthesis,
  it can be used as a normal function.

  All of the following plus_ functions can be used
  interchangeably.

----------------------------------------------------------------}


plusA x y = x + y
plusB x y = (+) x y

plusC = \x y -> x + y
plusD = (+)

main = asText $ (+) 4 7