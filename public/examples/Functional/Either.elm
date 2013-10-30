{--  Either  -----------------------------------------------

The either library is good for modeling values that can have
two different types.

A common use is for computations that may fail. Such values
are either an error message or the result (Either String a).
The result is normally held in the Right (a pun indicating
that it is correct).

More info at:
  http://elm-lang.org/docs/Data/Either.elm

-----------------------------------------------------------}

import open Either

names = [ Left "Alice", Right 43, Left "Bob", Right 29, Right 7 ]

main =
  flow down
    [ asText names,
      asText <| lefts names,
      asText <| rights names,
      asText <| partition names,
      asText <| map (either id show) names,
      asText <| map isLeft names,
      asText <| map isRight names ]
