-- More about the pipe operator (and related operators) here:
-- http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#|>

import Html exposing (text)
import String


isNotSpace char =
  char /= ' '


{- You will often end up calling a bunch of functions in a row.
Maybe something like this:
-}
weirdReversal1 string =
  String.filter isNotSpace (String.toUpper (String.reverse string))


{- Elm has the pipe operator (|>) that lets you write the same
exact function in a different way.

Think of the pipe as feeding a value into the next function. In
this case, we feed the string into reverse, then into toUpper,
then we filter out any numbers.
-}
weirdReversal2 string =
  string
    |> String.reverse
    |> String.toUpper
    |> String.filter isNotSpace


main =
  text (weirdReversal2 "s t r e s s e d")