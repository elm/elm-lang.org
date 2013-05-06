
{----------------------------------------------------------------

Overview:
  Function composition is just putting two functions together.

  Pretend you are in algebra again, and we have two functions,
  f(x) and g(x). Each takes a number and produces a number.
  Function composition combines these f and g into a new function
  we'll call h. We now have h such that h(x) = f(g(x)).

  The symbol for function composition is '.' and is meant to look
  like the standard mathematical notation for composition.

----------------------------------------------------------------}


square n = n * n
incr n = n + 1

incrThenSquare = square . incr

main = asText <| incrThenSquare 3