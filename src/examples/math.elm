import Html exposing (text)


-- You can use infix operators just like in normal math, so
-- when you want to know 2 + 2 you can write it just like that!
four =
  2 + 2


-- Multiplication is similar.
sixteen =
  8 * 2


-- When you start putting these together, it is good to add
-- parentheses sometimes.
eleven =
  (4 * 3) - 1


-- The infix operators follow the normal rules of math though
-- so you can leave off the parens if you want.
thirteen =
  4 * 3 + 1


-- There are some infix operators for boolean values. Say you
-- have two boolean values and you want to know if both of them
-- are true. You can use the AND operator, written &&
--
-- The following function takes a number called 'age' and checks
-- if it is greater than 12 and less than 20, all the numbers that
-- end in "teen"
isTeenage age =
  (age > 12) && (age < 20)


-- Again, infix operators are set up so they can be used together
-- in reasonable ways without parentheses, so 'isTeenage' can be
-- written like this:
isTeenageNoParens age =
  age > 12 && age < 20


-- When in doubt, add some parentheses!


main =
  text (
    toString
      [ isTeenage four
      , isTeenage sixteen
      , isTeenage eleven
      , isTeenage thirteen
      ]
  )