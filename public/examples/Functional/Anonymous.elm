
{----------------------------------------------------------------

Overview:
  In functional programming, functions are first class values.
  They can be passed as arguments and stored in data structures
  in exactly the same way as numbers or characters.

  It is possible 'anonymous' functions, functions that are not
  bound to any variable name. This often leads to more concise
  and clearer code.

----------------------------------------------------------------}


-- In traditional imperative languages, we'd have to define a
-- function before we could use it.

plus1 n = n + 1

-- This is actually syntactic sugar for the following:

plus1' = \n -> n + 1

-- plus1' is bound to a function that takes n as an argument and
-- then computes n + 1. If we only need to use this function
-- once, why bother binding it to a variable?

four = (\n -> n + 1) 3

main = asText four


-- This will become much more useful when we look at higher order
-- functions, functions that take functions as arguments.