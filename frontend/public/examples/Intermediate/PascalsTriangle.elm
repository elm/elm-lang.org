import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text
import Window


main =
  Varying.map triangle Window.width


triangle w =
  let style =
        toString >> Text.fromString >> Text.monospace >> centered >> width w
  in
      flow down (List.map style (pascals 8))

-- Try changing the value passed to 'pascals' above.


pascals depth =
  List.scanl (\_ lastLevel -> nextLevel lastLevel) [1] [1..depth-1]

nextLevel level =
  List.map2 (+) (0 :: level) (level ++ [0])
