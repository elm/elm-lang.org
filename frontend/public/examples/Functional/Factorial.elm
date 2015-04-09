import Graphics.Element exposing (Element, show)


factorial : Int -> Int
factorial n =
    if n <= 1
      then 1
      else n * factorial (n-1)


main : Element
main =
  show (factorial 5)