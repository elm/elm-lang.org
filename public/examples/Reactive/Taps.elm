-- Try this out on an iOS or Android device. For best results
-- use the "In Tab" compile option.

import Touch
import Window

scene (w,h) {x,y} =
    let positioned = move (toFloat x - toFloat w/2, toFloat h/2 - toFloat y) 
    in  collage w h [ positioned (filled teal (circle 40)) ]

main = lift2 scene Window.dimensions Touch.taps