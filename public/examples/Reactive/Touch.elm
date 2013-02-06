
-- Try this out on an iOS or Android device. For best results
-- use the "In Tab" compile option.

scene (w,h) = collage w h . map (\{x,y} -> outlined green $ circle 60 (x,y))
main = lift2 scene Window.dimensions Touch.touches


-- Force mobile devices to accurately report their dimensions:

foreign export jsevent "elm_viewport"
  content : Signal JSString

content = let c = "width=device-width, initial-scale=1"
          in  constant (JavaScript.castStringToJSString c)
