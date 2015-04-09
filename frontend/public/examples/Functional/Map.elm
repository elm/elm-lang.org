
{----------------------------------------------------------------

Overview:
  map takes two arguments: a function and a list. It applies the
  function to every element in the list, returning the result.

    map : (a -> b) -> List a -> List b

----------------------------------------------------------------}

import Graphics.Element exposing (show)


main =
  show (List.map (\n -> n^2) [1..5])