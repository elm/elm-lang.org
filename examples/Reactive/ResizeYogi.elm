
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Signal.Mouse (position)

resizeableYogi edgeLen = image edgeLen edgeLen "yogi.jpg"

edgeLen = lift (max 100 . uncurry max) position

main = lift resizeableYogi edgeLen