
scene (w,h) = container w h middle $ plainText "Hello, World!"

main = scene <~ Window.dimensions

-- Try changing the size of your browser window.