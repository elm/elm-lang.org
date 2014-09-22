
import Window

scene : (Int,Int) -> Element
scene (w,h) =
    container w h middle <| plainText "Hello, World!"

main : Signal Element
main = lift scene Window.dimensions

-- Try changing the size of your browser window.