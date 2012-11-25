
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

resizeableYogi edgeLen = image edgeLen edgeLen "yogi.jpg"

edgeLen = lift (max 100 . uncurry max) Mouse.position

main = lift resizeableYogi edgeLen