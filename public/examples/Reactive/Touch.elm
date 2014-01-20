-- Try this out on an iOS or Android device by removing "edit"
-- from the URL: http://elm-lang.org/examples/Reactive/Touch.elm

import Touch
import Window

makeCircle w h {x,y} =
    circle 60 |> filled green
              |> move (toFloat x - w/2, h/2 - toFloat y)

scene (w,h) = collage w h . map (makeCircle (toFloat w) (toFloat h))
main = lift2 scene Window.dimensions Touch.touches