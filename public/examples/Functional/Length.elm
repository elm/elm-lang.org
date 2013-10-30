
length xs =
    case xs of
      []     -> 0
      hd::tl -> 1 + length tl

main = asText (length [1..9])