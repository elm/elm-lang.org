
import Graphics.Input as Input

display box checked =
  container 30 30 middle box `beside` container 50 30 middle (asText checked)

main = let (box, checked) = Input.checkbox True
       in  lift2 display box checked