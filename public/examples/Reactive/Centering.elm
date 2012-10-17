
import Window (dimensions)

scene (w,h) = container w h middle $ plainText "Hello, World!"

main = lift scene dimensions

-- Try changing the size of your browser window.