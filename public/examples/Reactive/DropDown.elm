
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
          , ("Georgia"  , typeface "georgia, palatino, serif")
          ]

-- dropDown :: [(String,a)] -> (Element, Signal a)

(drop,choice) = Input.dropDown choices

main = lift (component drop) choice