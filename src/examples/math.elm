-- Check out https://guide.elm-lang.org/core_language.html
-- for a guided tour of features like this!

import Html exposing (text)


{- If you want to know 2 + 2, write it just like that!
-}
four =
  2 + 2


{- Multiplication is similar.
-}
sixteen =
  8 * 2


{- When you start putting these together, it can help to add
parentheses. The following means we definitely multiply four
and three before subtracting one.
-}
eleven =
  (4 * 3) - 1


{- These operators all follow the normal rules of math though,
so you can leave the parentheses off if you want.
-}
thirteen =
  4 * 3 + 1


{- There are some infix operators for boolean values. Say you
have two boolean values and you want to know if both of them
are true. You can use the AND operator, written &&

The following function takes a number called 'age' and checks
if it is greater than 12 and less than 20, all the numbers that
end in "teen"
-}
isTeenage age =
  (age > 12) && (age < 20)


{- Again, these operators are set up to work in a reasonable way
even without parentheses, so 'isTeenage' can be written like this.
-}
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