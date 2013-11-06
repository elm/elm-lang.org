
stylePairs =
    [ (bold            , "Bold")
    , (italic          , "Italicize")
    , (underline       , "Underline")
    , (Text.link "/"   , "Link")
    , (typeface "serif", "Typeface")
    , (Text.color red  , "Color")
    , (strikeThrough   , "Strike Through")
    , (overline        , "Overline")
    ]

pairToElement (style,name) = text (style (toText name))

main = flow down <| map pairToElement stylePairs
