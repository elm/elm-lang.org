import Color (..)
import Graphics.Element (..)
import Graphics.Input (dropDown)
import Signal
import Text


main : Signal Element
main =
  Signal.map display (Signal.subscribe style)


style : Signal.Channel (Text.Text -> Text.Text)
style =
  Signal.channel identity


display : (Text.Text -> Text.Text) -> Element
display transform =
  let msg = Text.fromString "Choose a style for the following text: "
  in
      flow down
        [ Text.leftAligned (msg ++ transform (Text.fromString "Hello, World!"))
        , dropDown options
        ]


options : List (String, Signal.Message)
options =
    [ option "plain"      identity
    , option "underline"  (Text.line Text.Under)
    , option "italic"     Text.italic
    , option "bold"       Text.bold
    , option "red"        (Text.color red)
    , option "monospace"  Text.monospace
    , option "Georgia"    (Text.typeface ["georgia", "palatino", "serif"])
    ]


option : String -> (Text.Text -> Text.Text) -> (String, Signal.Message)
option name transform =
  (name, Signal.send style transform)