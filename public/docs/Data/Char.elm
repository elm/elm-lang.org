
import Website.Docs (createDocs)

basics =
  [ ("toUpper", "Char -> Char", "Convert to upper case.")
  , ("toLower", "Char -> Char", "Convert to lower case.")
  , ("toLocaleUpper", "Char -> Char", "Convert to upper case, according to any locale-specific case mappings.")
  , ("toLocaleLower", "Char -> Char", "Convert to lower case, according to any locale-specific case mappings.")
  , ("toCode"  , "Char -> Int", "Convert to unicode.")
  , ("fromCode", "Int -> Char", "Convert from unicode.")
  ]

categories = [ ("Basic Utilities", basics) ]

main = createDocs "Data.Char" categories
