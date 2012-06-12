
import Website.Docs (createDocs)


creation =
  [ ("toText", "String -> Text", "Convert a string into text which can be styled and displayed.")
  , ("link", "String -> Text -> Text", "Create a link.")
  ]

textSize =
  [ ("header", "Text -> Text", "Makes text big and noticable.")
  , ("height", "Number -> Text -> Text", "Sets the height of text in \"ems\". 1em is the normal height of text. 2ems is twice that height.")
  ]

styles =
  [ ("italic", "Text -> Text", "Italicize a string.")
  , ("bold", "Text -> Text", "Make a string bold.")
  , ("underline", "Text -> Text", "Underline a string.")
  , ("overline", "Text -> Text", "Draw a line above a string.")
  , ("strikeThrough", "Text -> Text", "Draw a line through a string.")
  , ("color", "Color -> Text -> Text", "Set the color of a string.")
  , ("monospace", "Text -> Text", "Switch to a monospace typeface. Good for code snippets.")
  ]

categories = 
  [ ("Creating Text", creation)
  , ("Styling Text", styles)
  , ("Ajust Text Size", textSize)
  ]

main = createDocs "Text" categories