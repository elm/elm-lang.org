
{----------------------------------------------------------------

Overview:
  map takes two arguments: a function and a list. It applies the
  function to every element in the list, returning the result.

----------------------------------------------------------------}


main = asText <| map (\n -> n^2) [1..5]