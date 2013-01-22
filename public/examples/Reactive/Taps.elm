
-- Try this out on an iOS or Android device. For best results
-- use the "In Tab" compile option.

scene (w,h) {x,y} = collage w h [ outlined red (circle 40 (x,y)) ]

main = lift2 scene Window.dimensions Touch.taps