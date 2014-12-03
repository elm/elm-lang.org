import Graphics.Element (..)


-- opacity : Float -> Element -> Element

main : Element
main =
  opacity 0.5 (fittedImage 300 200 "/book.jpg")