
import Website.Docs (createDocs)

casts =
  [ ("castJSBoolToBool"       , "JSBool -> Bool", "Conversion from JavaScript boolean values to Elm boolean values.")
  , ("castBoolToJSBool"       , "Bool -> JSBool", "Conversion from Elm boolean values to JavaScript boolean values.")
  , ("castJSNumberToInt"      , "JSNumber -> Int", "")
  , ("castIntToJSNumber"      , "Int -> JSNumber", "")
  , ("castJSNumberToFloat"    , "JSNumber -> Float", "")
  , ("castFloatToJSNumber"    , "Float -> JSNumber", "")
  , ("castJSStringToString"   , "JSString -> String", "")
  , ("castStringToJSString"   , "String -> JSString", "")
  ]

polyCasts =
  [ ("castListToJSArray"      , "[a] -> JSArray a", "Produces a uniform JavaScript array with all members of the same type.")
  , ("castJSArrayToList"      , "JSArray a -> [a]", "Requires that the input array be uniform (all members have the same type)")
  , ("castTupleToJSTuple2"    , "(a,b) -> JSTuple2 a b", "A JSTupleN is an array of size N with nonuniform types. Each member can have a different type.")
  , ("castJSTupleToTuple2"    , "JSTuple2 a b -> (a,b)", "")
  , ("castTupleToJSTuple3"    , "(a,b,c) -> JSTuple3 a b c", "")
  , ("castJSTupleToTuple3"    , "JSTuple3 a b c > (a,b,c)", "")
  , ("castTupleToJSTuple4"    , "(a,b,c,d) -> JSTuple4 a b c d", "")
  , ("castJSTupleToTuple4"    , "JSTuple4 a b c d -> (a,b,c,d)", "")
  , ("castTupleToJSTuple5"    , "(a,b,c,d,e) -> JSTuple5 a b c d e", "")
  , ("castJSTupleToTuple5"    , "JSTuple5 a b c d e -> (a,b,c,d,e)", "")
  ]

categories =
  [ ("Basic Conversions", casts)
  , ("Conversion between Data Structures", polyCasts)
  ]

main = createDocs "Foreign.JavaScript" categories