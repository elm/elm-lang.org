
import Website.Docs (createDocs)

build =
  [ ("empty", "JsonObject a", "Create an empty JSON mapping.")
  , ("singleton", "String -> a -> JsonObject a", "Create a JSON mapping that contains only a single key-value pair.")
  , ("insert", "String -> a -> JsonObject a -> JsonObject a", "Add a key value pair to a JSON object.")
  , ("remove", "String -> JsonObject a -> JsonObject a", "Remove a key-value pair from a JsonObject.")
  ]

query =
  [ ("lookup", "String -> JsonObject a -> Maybe a", "Lookup a value in a JsonObject. If the key is not found, this returns Nothing.")
  , ("findString", "String -> JsonObject JsonValue -> String"
    , "Find a string value in a JsonObject. If the key is not found or the value found is not a string, this returns the empty string.")
  , ("findObject", "String -> JsonObject JsonValue -> JsonObject JsonValue"
    , "Find an object value in a JsonObject. If the key is not found or the value found is not an object, this returns an empty object.")
  , ("findArray", "String -> JsonObject JsonValue -> [JsonObject JsonValue]"
    , "Find an array value in a JsonObject. If the key is not found or the value found is not an array, this returns an empty list.")
  , ("findWithDefault", "a -> String -> JsonObject a -> a", "Find a value in a JsonObject. If the key is not found, this returns the given default value.")
  ]

lists =
  [ ("toList", "JsonObject a -> [(String,a)]", "Convert a JsonObject into an association list of key-value pairs.")
  , ("fromList", "[(String,a)] -> JsonObject a", "Convert an association list into a JsonObject.")
  ]

values =
  [ ("data JsonValue = JsonString String | JsonNumber Float | JsonBool Bool | JsonNull\n               | JsonArray [JsonValue] | JsonObject (JsonObject JsonValue)", ""
    , "This datatype can represent all valid values that can be held in a JSON object. In Elm, a proper JSON object is represented as a (JsonObject JsonValue) which is a mapping from strings to JsonValues. Note that the name &ldquo;JsonObject&rdquo; is used as a type constructor that creates JsonValues AND a completely separate type.")
  ]

strings =
  [ ("toString", "JsonObject JsonValue -> String", "Convert a proper JSON object (i.e. JsonObject JsonValue) into a string.")
  , ("fromString", "String -> JsonObject JsonValue", "Parse a string representation of a proper JSON object into its Elm's representation.")
  ]

jsStrings =
  [ ("toJSString", "JsonObject JsonValue -> JSString", "Convert a proper JSON object (i.e. JsonObject JsonValue) into a JavaScript string. Note that the type JSString seen here is not the same as the type constructor JsonString used elsewhere in this module.")
  , ("fromJSString", "JSString -> JsonObject JsonValue", "Parse a JavaScript string representation of a proper JSON object into its Elm's representation.")
  ]

prettyStrings =
  [ ("toPrettyString", "String -> JsonObject JsonValue -> String", "Convert a proper JSON object (i.e. JsonObject JsonValue) into a prettified string. The first argument is a separator token (e.g. \" \", \"\\n\", etc.) that will be used for indentation in the prettified string version of the JSON object.")
  , ("toPrettyJSString", "String -> JsonObject JsonValue -> JSString", "Same as toPrettyString except it produces a JavaScript string instead of an Elm string.")
  ]


categories =
  [ ("JSON Values", values)
  , ("Conversion", strings)
  , ("Query", query)
  , ("Pretty Strings", prettyStrings)
  , ("Build", build)
  , ("Lists", lists)
  , ("JavaScript Strings", jsStrings)
  ]

main = createDocs "JSON" categories