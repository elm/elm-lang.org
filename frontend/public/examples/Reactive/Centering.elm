import Graphics.Element (..)
import Signal
import Window
import Text (..)


main : Signal Element
main =
  Signal.map view Window.dimensions


view : (Int,Int) -> Element
view (w,h) =
  container w h middle (plainText "Hello, World!")


-- Try changing the size of your browser window.