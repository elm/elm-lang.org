import Mouse
import Window

main = lift2 slideShow Window.dimensions currentImage

slideShow (w,h) src =
  color black . container w h middle <| image 472 315 src

images = ["/book.jpg","/shells.jpg","/stack.jpg","/car.jpg","/pipe.jpg"]

currentImage =
  let cycler _ imgs = tail imgs ++ [head imgs]
  in  head <~ foldp cycler images (every (2*second))


-- try replacing (every (2*second)) with Mouse.clicks