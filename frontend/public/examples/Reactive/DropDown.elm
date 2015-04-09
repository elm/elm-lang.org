import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (dropDown)
import Text


main : Signal Element
main =
  Signal.map display style.signal


style : Signal.Mailbox (Text.Text -> Text.Text)
style =
  Signal.mailbox identity


display : (Text.Text -> Text.Text) -> Element
display transform =
  let msg = Text.fromString "Choose a style for the following text: "
  in
      flow down
        [ leftAligned (msg ++ transform (Text.fromString "Hello, World!"))
        , dropDown (Signal.message style.address) options
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