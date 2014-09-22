
-- Show an image that resizes to fit the window
-- while maintaining its aspect ratio.

import Window

resizeablePaint : (Int,Int) -> Element
resizeablePaint (w,h) =
    fittedImage w h "/paint.jpg"

main : Signal Element
main =
    lift resizeablePaint Window.dimensions



-- Try resizing the demo pane. For best results, compile this in a 'New Tab' or
-- in 'Full-Screen' mode and try playing with the dimensions of your browser.