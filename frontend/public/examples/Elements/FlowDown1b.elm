import Graphics.Element exposing (..)


main : Element
main =
  flow down
    (List.map plainText [ "line 1", "line 2", "line 3" ])