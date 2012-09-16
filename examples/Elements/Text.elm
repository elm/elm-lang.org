
import Graphics.Text as Text

main = flow down $ map (\(f,t) -> text . f $ toText t)
    [ (header          , "Header")
    , (bold            , "Bold")
    , (italic          , "Italicize")
    , (underline       , "Underline")
    , (Text.link "/"   , "Link")
    , (typeface "serif", "Typeface")
    , (Text.color red  , "Color")
    , (strikeThrough   , "Strike Through")
    , (overline        , "Overline")
    ]