
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Graphics.Element (..)
import Mouse
import Signal


main : Signal Element
main =
  Signal.map resizeableYogi edgeLength


resizeableYogi : Int -> Element
resizeableYogi n =
  image n n "/yogi.jpg"


edgeLength : Signal Int
edgeLength =
  Signal.map (\(x,y) -> max x y) Mouse.position
