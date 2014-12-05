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
        , dropDown (Signal.send style) options
        ]


options : List (String, Text.Text -> Text.Text)
options =
    [ "plain"     := identity
    , "underline" := Text.line Text.Under
    , "italic"    := Text.italic
    , "bold"      := Text.bold
    , "red"       := Text.color red
    , "monospace" := Text.monospace
    , "Georgia"   := Text.typeface ["georgia", "palatino", "serif"]
    ]


(:=) name transform =
    (name, transform)