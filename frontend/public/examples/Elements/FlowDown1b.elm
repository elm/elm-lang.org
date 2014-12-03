import Graphics.Element (..)
import List
import Text (..)

main : Element
main =
  flow down
    (List.map plainText [ "line 1", "line 2", "line 3" ])