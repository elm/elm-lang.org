module Website.BigTiles (examples, example) where

import Graphics.Input as Input
import Text
import Website.ColorScheme as C

examples w exs =
    let block = flow down . intersperse (spacer 20 20) <| map row exs
    in  container w (heightOf block) middle block

row = flow right . intersperse (spacer 20 200) . map (example (200,200))

clicks : Input.Input ()
clicks = Input.input ()

sourceCode clr =
    container 200 20 middle . centered . Text.color clr . Text.height 10 <| toText "Source Code"

example (w,h) (picture, demo, source) =
    let btn ext =
            image w h ("/screenshot/" ++ picture ++ "." ++ ext)

        sourceLink =
            case source of
              Nothing -> spacer 200 20
              Just src -> link src <|
                          Input.customButton clicks.handle () (sourceCode C.mediumGrey) (sourceCode lightCharcoal) (sourceCode lightCharcoal)

    in  flow down
        [ link demo <|
          Input.customButton clicks.handle () (btn "png") (btn "gif") (btn "png")
        , sourceLink
        ]

