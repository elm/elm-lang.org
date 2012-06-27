
import Website.Docs (createDocs)

casts =
  [ ("castJSBoolToBool"       , "JSBool -> Bool", "")
  , ("castBoolToJSBool"       , "Bool -> JSBool", "")
  , ("castJSNumberToInt"      , "JSNumber -> Int", "")
  , ("castIntToJSNumber"      , "Int -> JSNumber", "")
  , ("castJSElementToElement" , "JSElement -> Element", "")
  , ("castElementToJSElement" , "Element -> JSElement", "")
  , ("castJSStringToString"   , "JSString -> String", "")
  , ("castStringToJSString"   , "String -> JSString", "")
  , ("castJSNumberToFloat"    , "JSNumber -> Float", "")
  , ("castFloatToJSNumber"    , "Float -> JSNumber", "")
  , ("castJSArrayToList"      , "JSArray a -> listOf a", "")
  , ("castListToJSArray"      , "listOf a -> JSArray a", "")
  , ("castTupleToJSTuple2"    , "(a,b) -> JSTuple2 a b", "")
  , ("castTupleToJSTuple3"    , "(a,b,c) -> JSTuple3 a b c", "")
  , ("castTupleToJSTuple4"    , "(a,b,c,d) -> JSTuple4 a b c d", "")
  , ("castTupleToJSTuple5"    , "(a,b,c,d,e) -> JSTuple5 a b c d e", "")
  , ("castJSTupleToTuple2"    , "JSTuple2 a b -> (a,b)", "")
  , ("castJSTupleToTuple3"    , "JSTuple3 a b c > (a,b,c)", "")
  , ("castJSTupleToTuple4"    , "JSTuple4 a b c d -> (a,b,c,d)", "")
  , ("castJSTupleToTuple5"    , "JSTuple5 a b c d e -> (a,b,c,d,e)", "")
  ]

categories = [ ("Converting to and from JavaScript types", casts) ]

main = createDocs "Foreign.JavaScript" categories