import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Time exposing (second)
import Window


main : Varying Element
main =
  Varying.map2 slideShow Window.dimensions currentImage


slideShow : (Int, Int) -> String -> Element
slideShow (w,h) src =
  image 472 315 src
    |> container w h middle
    |> color black

images : List String
images =
  [ "/book.jpg", "/shells.jpg", "/stack.jpg", "/car.jpg", "/pipe.jpg" ]

currentImage : Varying String
currentImage =
  let cycler _ imgs = tail imgs ++ [head imgs]
  in
      Varying.map head (Signal.fold cycler images (Time.every (2*second)))


-- try replacing (every (2*second)) with Mouse.clicks