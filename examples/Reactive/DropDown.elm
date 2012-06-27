
import Signal.Input (dropDown)

component dropDown style =
  let msg = toText "Choose a style for the following text: " ++
            style (toText "Hello, World!")
  in
      text msg `above` dropDown

-- choices :: [(String, Text -> Text)]

choices = [ ("underline", underline)
          , ("italic"   , italic)
          , ("bold"     , bold)
          , ("red"      , Text.color red)
          , ("monospace", monospace)
          ]

-- dropDown :: [(String,a)] -> (Element, Signal a)

main = let (drop,choice) = dropDown choices in
       lift (component drop) choice