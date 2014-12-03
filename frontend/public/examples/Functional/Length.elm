
import List ((::))
import Text (asText)


length : List a -> Int
length list =
  case list of
    [] -> 0

    first :: rest ->
      1 + length rest


main =
  asText (length [1..9])