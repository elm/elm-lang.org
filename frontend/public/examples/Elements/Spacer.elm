import Color exposing (red)
import Graphics.Element exposing (Element, color, spacer)


-- A spacer is just an empty box. It is nice for making spaces!

main : Element
main =
  color red (spacer 30 30)