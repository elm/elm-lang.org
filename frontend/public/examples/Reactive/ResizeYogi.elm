
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Mouse

edgeLength : Signal Int
edgeLength =
    lift (\(x,y) -> max x y) Mouse.position

resizeableYogi : Int -> Element
resizeableYogi n =
    image n n "/yogi.jpg"

main : Signal Element
main =
    lift resizeableYogi edgeLength