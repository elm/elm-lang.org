-- Try this out on an iOS or Android device by removing "edit"
-- from the URL: http://elm-lang.org/examples/Reactive/Taps.elm

import Touch
import Window

scene (w,h) {x,y} =
    let positioned = move (toFloat x - toFloat w/2, toFloat h/2 - toFloat y) 
    in  collage w h [ positioned (filled purple (circle 40)) ]

main = lift2 scene Window.dimensions Touch.taps