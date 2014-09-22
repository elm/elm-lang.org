import Text

main : Element
main =
    flow down
        [ leftAligned (bold (toText "Bold"))
        , leftAligned (italic (toText "Italicize"))
        , leftAligned (Text.link "/" (toText "Link"))
        ]

-- Challenge: can you rewrite this example so the code is less
-- repetative? Try using map to factor out common patterns.