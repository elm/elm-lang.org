import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Time exposing (..)
import Window


main : Signal Element
main =
  Signal.map2 slideShow Window.dimensions currentImage


slideShow : (Int, Int) -> String -> Element
slideShow (w,h) src =
  image 472 315 src
    |> container w h middle
    |> color black

images : List String
images =
  [ "/book.jpg", "/shells.jpg", "/stack.jpg", "/car.jpg", "/pipe.jpg" ]

currentImage : Signal String
currentImage =
  let cycler _ (head::tail) =
        tail ++ [head]

      head list =
        Maybe.withDefault "/book.jpg" (List.head list)
  in
      Signal.map head (Signal.foldp cycler images (every (2*second)))


-- try replacing (every (2*second)) with Mouse.clicks