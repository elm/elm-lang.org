
import Website.Docs (createDocs)

classification =
  [ ("isUpper", "Char -> Bool", "Selects upper case letters.")
  , ("isLower", "Char -> Bool", "Selects lower case letters.")
  , ("isDigit", "Char -> Bool", "Selects ASCII digits (0..9).")
  , ("isOctDigit", "Char -> Bool", "Selects ASCII octal digits (0..7).")
  , ("isHexDigit", "Char -> Bool"
    , "Selects ASCII hexadecimal digits (0..9a..fA..F).")
  ]

conversion =
  [ ("toUpper", "Char -> Char", "Convert to upper case.")
  , ("toLower", "Char -> Char", "Convert to lower case.")
  , ("toLocaleUpper", "Char -> Char"
    , "Convert to upper case, according to any locale-specific case mappings.")
  , ("toLocaleLower", "Char -> Char"
    , "Convert to lower case, according to any locale-specific case mappings.")
  , ("toCode"  , "Char -> Int", "Convert to unicode.")
  , ("fromCode", "Int -> Char", "Convert from unicode.")
  ]

categories =
  [ ("Classification", classification)
  , ("Conversion", conversion)
  ]

main = createDocs "Char" categories
