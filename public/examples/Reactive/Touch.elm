
scene (w,h) = collage w h . map (\{x,y} -> outlined green $ circle 60 (x,y))

main = lift2 scene Window.dimensions Touch.touches


content = constant (JavaScript.castStringToJSString
                    "width=device-width, initial-scale=1")
foreign export jsevent "elm_viewport"
  content :: Signal JSString