
component dropDown style =
  flow down [ text $ toText "Choose a style for the following text: " ++
                     style (toText "Hello, World!")
            , dropDown ]

-- choices :: [(String, Text -> Text)]

choices = [ ("underline", underline)
          , ("italic"   , italic)
          , ("bold"     , bold)
          , ("red"      , Text.color red)
          , ("monospace", monospace)
          ]

-- dropDown :: [(String,a)] -> (Element, Signal a)

main = case Input.dropDown choices of
       { (drop, choice) -> lift (component drop) choice }