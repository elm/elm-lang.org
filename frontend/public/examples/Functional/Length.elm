
import List exposing ((::))
import Text exposing (asText)


length : List a -> Int
length list =
  case list of
    [] -> 0

    first :: rest ->
      1 + length rest


main =
  asText (length [1..9])