
import Graphics.Input as Input

main = let (box, checked) = Input.checkbox True
       in  lift2 display box checked

display box checked =
  flow right [ container 30 30 middle box,
               container 50 30 middle (asText checked) ]

