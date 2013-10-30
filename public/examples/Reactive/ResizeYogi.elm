
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Mouse

edgeLength = lift (uncurry max) Mouse.position

resizeableYogi n = image n n "/yogi.jpg"

main = lift resizeableYogi edgeLength