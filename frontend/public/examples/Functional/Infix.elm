
{----------------------------------------------------------------

Overview:
  Sometimes it is useful to use an infix operator such as
  addition (+) or cons (::) as a function.

  In Elm, when an infix operator is surrounded by parenthesis,
  it can be used as a normal function.

  All of the following `plus` functions can be used
  interchangeably.

----------------------------------------------------------------}

import Graphics.Element exposing (show)


plusA x y = x + y

plusB x y = (+) x y

plusC = \x y -> x + y

plusD = (+)


main =
  show ((+) 4 7)
