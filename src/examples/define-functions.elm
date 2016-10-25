-- Check out https://guide.elm-lang.org/core_language.html
-- for a guided tour of features like this!

import Html exposing (text)


{- To define a function, provide a name and then list all the
arguments separated by spaces. The 'add' function takes in two
arguments 'x' and 'y' and adds them together.
-}
add x y =
  x + y


{- The 'factorial' function takes in a number 'n' and computes
the factorial by multiplying (1 * 2 * 3 * ... * n)
-}
factorial n =
  List.product (List.range 1 n)


{- The 'main' value needs to evaluate all of our functions to
know what to show on screen. It will step through things like
this:

    add 1 (factorial 4)
    add 1 (List.product (List.range 1 4))
    add 1 (List.product [1, 2, 3, 4])
    add 1 24
    1 + 24
    25

Notice that a function is like find-and-replace. Whenever we
see 'factorial' we replace it with the definition.
-}
main =
  text (toString (add 1 (factorial 4)))
