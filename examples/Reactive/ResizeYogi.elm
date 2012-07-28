
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

import Signal.Mouse (position)

resizeableYogi edgeLen = size edgeLen edgeLen (image "yogi.jpg")
main = lift (resizeableYogi . uncurry max) position