-- Check out https://guide.elm-lang.org/core_language.html
-- for a guided tour of features like this!

import Html exposing (text)
import String


{- This function will take a string and try to capitalize the first
letter. It is kind of tough to read since we are doing so many
operations in just one line!
-}
capitalize1 word =
  String.toUpper (String.left 1 word) ++ String.dropLeft 1 word


{- So here is another version that uses a let-expression to make
things a bit clearer. Notice the `let` and `in` keywords. This is
like saying, LET `firstLetter` and `otherLetters` be defined IN the
following expression, but nowhere else.
-}
capitalize2 word =
  let
    firstLetter =
      String.left 1 word

    otherLetters =
      String.dropLeft 1 word
  in
    String.toUpper firstLetter ++ otherLetters


main =
  text (capitalize2 "who ate all the pie?")
