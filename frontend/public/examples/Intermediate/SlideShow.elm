import Mouse
import Window

main : Signal Element
main = lift2 slideShow Window.dimensions currentImage

slideShow : (Int, Int) -> String -> Element
slideShow (w,h) src =
    image 472 315 src
      |> container w h middle
      |> color black

images : [String]
images =
    [ "/book.jpg", "/shells.jpg", "/stack.jpg", "/car.jpg", "/pipe.jpg" ]

currentImage : Signal String
currentImage =
    let cycler _ imgs = tail imgs ++ [head imgs]
    in  head <~ foldp cycler images (every (2*second))


-- try replacing (every (2*second)) with Mouse.clicks