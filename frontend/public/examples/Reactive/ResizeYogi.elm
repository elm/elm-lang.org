
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Graphics.Element exposing (..)
import Mouse


main : Varying Element
main =
  Varying.map resizeableYogi edgeLength


resizeableYogi : Int -> Element
resizeableYogi n =
  image n n "/yogi.jpg"


edgeLength : Varying Int
edgeLength =
  Varying.map (\(x,y) -> max x y) Mouse.position
