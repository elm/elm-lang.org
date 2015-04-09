
import Graphics.Element exposing (show)


length : List a -> Int
length list =
  case list of
    [] -> 0

    first :: rest ->
      1 + length rest


main =
  show (length [1..9])