

-- Show an image that resizes to fit the window
-- while maintaining its aspect ratio.

resizeablePaint (w,h) = fittedImage w h "paint.jpg"
main = resizeablePaint <~ Window.dimensions



-- Try resizing the demo pane. For best results, compile this in a 'New Tab' or
-- in 'Full-Screen' mode and try playing with the dimensions of your browser.