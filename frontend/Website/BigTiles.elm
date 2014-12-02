module Website.BigTiles (examples, example) where

import Color
import Graphics.Element (..)
import Graphics.Input as Input
import List
import Signal
import Text
import Website.ColorScheme as C


examples w exs =
  let block =
        List.map row exs
          |> List.intersperse (spacer 20 20)
          |> flow down
  in
      container w (heightOf block) middle block

row examples =
  List.map (example (200,200)) examples
    |> List.intersperse (spacer 20 200)
    |> flow right


clicks : Signal.Channel String
clicks = Signal.channel ""


--bad = lift Native.RedirectHack.redirect clicks.signal


sourceCode clr =
  Text.fromString "Source Code"
    |> Text.height 10
    |> Text.color clr
    |> Text.centered
    |> container 200 20 middle


example (w,h) (picture, demo, source) =
    let btn ext =
            image w h ("/screenshot/" ++ picture ++ "." ++ ext)

        sourceLink =
            case source of
              Nothing -> spacer 200 20
              Just src ->
                  link src <|
                    Input.customButton
                        (Signal.send clicks src)
                        (sourceCode C.mediumGrey)
                        (sourceCode Color.lightCharcoal)
                        (sourceCode Color.lightCharcoal)

    in  flow down
        [ link demo <|
          Input.customButton (Signal.send clicks demo) (btn "png") (btn "gif") (btn "png")
        , sourceLink
        ]

