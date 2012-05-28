
{----------------------------------------------------------------

Overview:
  map takes two arguments: a function and a list. It applies the
  function to every element in the list, returning the result.

----------------------------------------------------------------}


main = asText $ List.map (\n -> n * n) [1..5]