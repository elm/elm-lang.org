

-- Show an image that resizes to fit the window
-- while maintaining its aspect ratio.

resizeablePaint edgeLen = size edgeLen edgeLen $ image "paint.jpg"

uncurry f (x,y) = f x y

main = lift (resizeablePaint . uncurry min) Window.dimensions



-- Try resizing the demo pane. For best results, compile this in a 'New Tab' or
-- in 'Full-Screen' mode and try playing with the dimensions of your browser.