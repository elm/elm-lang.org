
main = flow down $ List.map (\(f,t) -> text . f $ toText t)
    [ (header         , "Header")
    , (bold           , "Bold")
    , (italic         , "Italicize")
    , (underline      , "Underline")
    , (link "/"       , "Link")
    , (strikeThrough  , "Strike Through")
    , (Text.color red , "Color")
    , (overline       , "Overline")
    ]