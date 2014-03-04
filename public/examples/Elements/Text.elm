
main : Element
main = flow down <| map pairToElement stylePairs

stylePairs : [(String, Text -> Text)]
stylePairs =
    [ ("Bold"     , bold)
    , ("Italicize", italic)
    , ("Underline", line Under)
    , ("Link"     , Text.link "/")
    , ("Typeface" , typeface ["serif"])
    , ("Color"    , Text.color red)
    , ("Strikeout", line Through)
    , ("Overline" , line Over)
    ]

pairToElement : (String, Text -> Text) -> Element
pairToElement (name, style) =
    leftAligned (style (toText name))
