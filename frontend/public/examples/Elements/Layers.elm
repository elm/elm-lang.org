import Graphics.Element exposing (..)
import Text


main : Element
main =
  layers
    [ fittedImage 320 240 "/shells.jpg"
    , width 320 (centered (Text.fromString "She sells sea shells."))
    ]
