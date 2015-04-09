module Website.BigTiles (examples, example) where

import Color
import Graphics.Element exposing (..)
import Graphics.Input as Input
import Native.RedirectHack
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


clicks : Signal.Mailbox String
clicks =
  Signal.mailbox ""


bad =
  Signal.map Native.RedirectHack.redirect clicks.signal


sourceCode clr =
  Text.fromString "Source Code"
    |> Text.height 10
    |> Text.color clr
    |> centered
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
                        (Signal.message clicks.address src)
                        (sourceCode C.mediumGrey)
                        (sourceCode Color.lightCharcoal)
                        (sourceCode Color.lightCharcoal)

    in  flow down
        [ link demo <|
          Input.customButton (Signal.message clicks.address demo) (btn "png") (btn "gif") (btn "png")
        , sourceLink
        ]

