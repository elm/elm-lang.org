module Website.Tiles (examples, intermediate, webgl) where

import Graphics.Input as Input
import Website.ColorScheme (..)

examples w exs =
    let block = flow down . intersperse (spacer 10 10) <| map row exs
    in  container w (heightOf block) middle block

row = flow right . intersperse (spacer 10 124) . map example

clicks : Input.Input ()
clicks = Input.input ()

intermediate name =
    ("Intermediate/" ++ name, "/edit/examples/Intermediate/" ++ name ++ ".elm")

webgl name =
    ("WebGL/" ++ name, "/edit/examples/WebGL/" ++ name ++ ".elm")

example (picture, location) =
    let btn clr = color clr . container 124 124 middle <|
                  image 120 120 ("/screenshot/" ++ picture ++ ".jpg")
    in  link location <|
        Input.customButton clicks.handle () (btn mediumGrey) (btn accent1) (btn accent3)
