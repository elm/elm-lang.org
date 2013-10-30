
import Window

scene (w,h) = container w h middle <| plainText "Hello, World!"

main = lift scene Window.dimensions

-- Try changing the size of your browser window.