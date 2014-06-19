module Website.BigTiles (examples, example) where

import Graphics.Input as Input
import Website.ColorScheme (..)

examples w exs =
    let block = flow down . intersperse (spacer 20 20) <| map row exs
    in  container w (heightOf block) middle block

row = flow right . intersperse (spacer 20 200) . map (example (200,200))

clicks : Input.Input ()
clicks = Input.input ()

example (w,h) (picture, location) =
    let btn ext = image w h ("/screenshot/" ++ picture ++ "." ++ ext)
    in  link location <|
        Input.customButton clicks.handle () (btn "png") (btn "gif") (btn "png")
