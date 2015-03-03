import Graphics.Element exposing (..)
import List
import Text exposing (..)

main : Element
main =
  flow down
    (List.map plainText [ "line 1", "line 2", "line 3" ])