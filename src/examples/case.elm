-- Case-expressions get way more useful once you learn about union types.
-- Do that by reading the following section of the guide:
--
--    https://guide.elm-lang.org/types/union_types.html

import Html exposing (text)
import String


{- A case-expression lets you match particular values. The following
function has 5 cases. If the incoming `number` is a 1, we will output
"one".

Notice that the last case is an underscore. We call that a "wildcard"
and it will match anything.
-}
primitiveCount number =
  case number of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    _ -> "many"


{- Again, case-expressions get way more interesting when you are
using union types, so be sure to read about them here:

    https://guide.elm-lang.org/types/union_types.html
-}


main =
  text (String.join ", " primitiveNumbers)


primitiveNumbers =
  [ primitiveCount 1
  , primitiveCount 2
  , primitiveCount 3
  , primitiveCount 4
  , primitiveCount 5
  , primitiveCount 6
  , primitiveCount 7
  ]