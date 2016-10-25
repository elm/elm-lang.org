-- For a full introduction to types, read this section of the guide:
-- https://guide.elm-lang.org/types/

import Html exposing (text)


{- Here we are defining an `increment` function that adds one to a
number. The only change is we have this extra "type annotation" above
the definition.

It is saying, this function takes in an integer and returns and integer.
-}
increment : Int -> Int
increment n =
  n + 1


{- Now we define `isNear` which takes two numbers and figures out if
they are nearby. The definition is like normal, but again, we add a
type annotation on the line above.

Sometimes folks are weirded out by seeing multiple arrows here. You can
think of it as "Everything is an argument until after the last arrow"
when you are learning. To learn what is REALLY going on, check out:

    https://guide.elm-lang.org/types/reading_types.html
-}
isNear : Int -> Int -> Bool
isNear x y =
  abs (x - y) < 10


{- Type annotations are ALWAYS optional. The compiler can figure out
all the types in your program without any annotations.

Nonetheless, most Elm code has type annotations for a few reasons:

  - They improve error messages during development.
  - They make it easier to read the code later.
  - They help you understand EXACTLY what your functions do.

So you can get pretty far with Elm without understanding types very
well, but it is worth investing some time in them. They are surprisingly
helpful once you get used to them! Again, do that by reading:

    https://guide.elm-lang.org/types/reading_types.html
-}


main =
  text (toString (isNear 72 (increment 75)))