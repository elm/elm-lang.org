
scene (w,h) {x,y} = collage w h [ outlined red (circle 40 (x,y)) ]

main = lift2 scene Window.dimensions Touch.taps