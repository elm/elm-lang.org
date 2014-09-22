
import Graphics.Input (Input, input, dropDown)
import Text

main : Signal Element
main =
    lift display style.signal

style : Input (Text -> Text)
style = input identity

display : (Text -> Text) -> Element
display transform =
    let msg = toText "Choose a style for the following text: "
    in
        flow down
            [ leftAligned (msg ++ transform (toText "Hello, World!"))
            , dropDown style.handle options
            ]

options : [(String, Text -> Text)]
options =
    [ ("plain"    , identity)
    , ("underline", Text.line Text.Under)
    , ("italic"   , italic)
    , ("bold"     , bold)
    , ("red"      , Text.color red)
    , ("monospace", monospace)
    , ("Georgia"  , typeface ["georgia", "palatino", "serif"])
    ]

