import Graphics.Element exposing (..)
import Window
import Text exposing (..)


main : Varying Element
main =
  Varying.map view Window.dimensions


view : (Int,Int) -> Element
view (w,h) =
  container w h middle (plainText "Hello, World!")


-- Try changing the size of your browser window.