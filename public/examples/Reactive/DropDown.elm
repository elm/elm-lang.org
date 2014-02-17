
import Graphics.Input as Input

main : Signal Element
main = lift display choice

(choice, portal) = Input.input id

display : (Text -> Text) -> Element
display style =
  let msg = toText "Choose a style for the following text: " in
  flow down [ text (msg ++ style (toText "Hello, World!"))
            , Input.dropDown portal options
            ]

options : [(String, Text -> Text)]
options = [ ("plain"    , id)
          , ("underline", underline)
          , ("italic"   , italic)
          , ("bold"     , bold)
          , ("red"      , Text.color red)
          , ("monospace", monospace)
          , ("Georgia"  , typeface "georgia, palatino, serif")
          ]

