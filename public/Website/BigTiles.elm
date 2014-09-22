module Website.BigTiles (examples, example) where

import Graphics.Input as Input
import Text
import Website.ColorScheme as C
import Native.RedirectHack

examples w exs =
    let block = flow down << intersperse (spacer 20 20) <| map row exs
    in  container w (heightOf block) middle block

row = flow right << intersperse (spacer 20 200) << map (example (200,200))

clicks : Input.Input String
clicks = Input.input ""

bad = lift Native.RedirectHack.redirect clicks.signal

sourceCode clr =
    container 200 20 middle << centered << Text.color clr << Text.height 10 <| toText "Source Code"

example (w,h) (picture, demo, source) =
    let btn ext =
            image w h ("/screenshot/" ++ picture ++ "." ++ ext)

        sourceLink =
            case source of
              Nothing -> spacer 200 20
              Just src -> link src <|
                          Input.customButton clicks.handle src (sourceCode C.mediumGrey) (sourceCode lightCharcoal) (sourceCode lightCharcoal)

    in  flow down
        [ link demo <|
          Input.customButton clicks.handle demo (btn "png") (btn "gif") (btn "png")
        , sourceLink
        ]

