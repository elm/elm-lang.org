import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List (..)
import Signal
import Text
import Window


main =
  Signal.map triangle Window.width


triangle w =
  let style =
        toString >> Text.fromString >> Text.monospace >> Text.centered >> width w
  in
      flow down (map style (pascals 8))

-- Try changing the value passed to 'pascals' above.


pascals depth =
  scanl (\_ lastLevel -> nextLevel lastLevel) [1] [1..depth-1]

nextLevel level =
  map2 (+) (0 :: level) (level ++ [0])
