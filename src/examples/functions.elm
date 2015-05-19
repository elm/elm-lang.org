import Graphics.Element exposing (show)


-- The 'sqrt' function takes one argument. It figures out
-- the square root of a number.
four =
  sqrt 16


-- The 'max' function takes two arguments. It tells you which
-- of the two arguments is bigger.
eleven =
  max 2 11


-- If the arguments are more complex, we put them in parentheses
-- to make the grouping more clear. When the following expression
-- gets evaluated, it goes like this:
--
--     max (sqrt 100) (4 * 5)
--     max 10 (4 * 5)
--     max 10 20
--     20
--
twenty =
  max (sqrt 100) (4 * 5)


-- The 'show' function takes one argument. It turns any value into
-- something we can show on screen. In this case, we are giving a
-- list of numbers.
main =
  show [four, eleven, twenty]