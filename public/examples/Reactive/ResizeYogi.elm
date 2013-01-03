
-- Show an image of Yogi that resizes while maintaining its aspect ratio.

resizeableYogi edgeLen = image edgeLen edgeLen "yogi.jpg"

edgeLen = (max 100 . uncurry max) <~ Mouse.position

main = resizeableYogi <~ edgeLen