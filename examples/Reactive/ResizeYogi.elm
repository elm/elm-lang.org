

-- Show an image of Yogi that resizes while maintaining its aspect ratio.

resizeableYogi edgeLen = size edgeLen edgeLen $ image "yogi.jpg"

uncurry f (x,y) = f x y

main = lift (resizeableYogi . uncurry max) Mouse.position