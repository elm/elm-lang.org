-- Check out https://guide.elm-lang.org/core_language.html
-- for a guided tour of features like this!

import Html exposing (text)


{- We are defining a function called isOver9000. It takes an integer
as its only argument, and then uses an "if-expression" to figure out
if it is greater than 9000.
-}
isOver9000 powerLevel =
  if powerLevel > 9000 then "It's over 9000!!!" else "meh"


assessments =
  [ isOver9000 5
  , isOver9000 1234
  , isOver9000 6789
  , isOver9000 9999
  ]


main =
  text (toString assessments)


{-| But maybe we want a more nuanced review of power levels. So we are
defining the assessPowerLevel function that will give different
assessments based on how close you are to 9000.

Try swapping out `isOver9000` for `assessPowerLevel` in the definition
of `assessments` above. I want to see the the nicer reviews!
-}
assessPowerLevel powerLevel =
  if powerLevel > 9000 then
    "It's over 9000!!!"

  else if powerLevel > 6000 then
    "That is pretty high!"

  else if powerLevel > 1000 then
    "Good effort."

  else
    "Are you even trying?"
