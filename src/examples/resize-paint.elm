
-- Show an image that resizes to fit the window
-- while maintaining its aspect ratio.

import Graphics.Element exposing (..)
import Window


main : Signal Element
main =
  Signal.map resizeablePaint Window.dimensions


resizeablePaint : (Int,Int) -> Element
resizeablePaint (w,h) =
  fittedImage w h "/paint.jpg"



-- Try resizing the demo pane. For best results, compile this in a 'New Tab' or
-- in 'Full-Screen' mode and try playing with the dimensions of your browser.