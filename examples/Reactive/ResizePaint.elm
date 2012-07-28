

-- Show an image that resizes to fit the window
-- while maintaining its aspect ratio.

import Signal.Window (dimensions)

resizeablePaint edgeLen = size edgeLen edgeLen (image "paint.jpg")
main = lift (resizeablePaint . uncurry min) dimensions



-- Try resizing the demo pane. For best results, compile this in a 'New Tab' or
-- in 'Full-Screen' mode and try playing with the dimensions of your browser.