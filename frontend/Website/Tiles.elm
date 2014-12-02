module Website.Tiles (examples, intermediate, webgl) where

import Graphics.Element (..)
import Graphics.Input as Input
import List
import Signal
import Website.ColorScheme (..)


examples w exs =
  let block =
        List.map row exs
          |> List.intersperse (spacer 10 10)
          |> flow down
  in
    container w (heightOf block) middle block


row examples =
  List.map example examples
    |> List.intersperse (spacer 10 124)
    |> flow right


clicks : Signal.Channel String
clicks =
  Signal.channel ""


--bad = lift Native.RedirectHack.redirect clicks.signal

intermediate name =
  ("Intermediate/" ++ name, "/edit/examples/Intermediate/" ++ name ++ ".elm")


webgl name =
  ("WebGL/" ++ name, "/edit/examples/WebGL/" ++ name ++ ".elm")

example (picture, location) =
  let btn clr =
        image 120 120 ("/screenshot/" ++ picture ++ ".jpg")
          |> container 124 124 middle
          |> color clr
  in
    link location <|
      Input.customButton
          (Signal.send clicks location)
          (btn mediumGrey)
          (btn accent1)
          (btn accent3)
