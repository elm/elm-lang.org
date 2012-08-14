
import Website.Docs (createDocs)

objects =
  [ ("empty", "JsonObject a", "Create an empty JSON mapping.")
  , ("singleton", "String -> a -> JsonObject a", "Create a JSON mapping that contains only a single key-value pair.")
  , ("insert", "String -> a -> JsonObject a -> JsonObject a", "Add a key value pair to a JSON object.")
  , ("lookup", "String -> JsonObject a -> Maybe a", "Lookup a value in a JsonObject. If the key is not found, this returns Nothing.")
  , ("findWithDefault", "a -> String -> JsonObject a -> a", "Find a value in a JsonObject. If the key is not found, this returns the given default value.")
  , ("remove", "String -> JsonObject a -> JsonObject a", "Remove a key-value pair from a JsonObject.")
  ]

lists =
  [ ("toList", "JsonObject a -> [(String,a)]", "Convert a JsonObject into an association list of key-value pairs.")
  , ("fromList", "[(String,a)] -> JsonObject a", "Convert an association list into a JsonObject.")
  ]

values =
  [ ("data JsonValue = JsonString String | JsonNumber Float | JsonBool Bool | JsonNull | JsonArray [JsonValue] | JsonObject (JsonObject JsonValue)", ""
    , "This datatype can represent all valid values that can be held in a JSON object. In Elm, a proper JSON object is represented as a (JsonObject JsonValue) which is a mapping from strings to JsonValues.")
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
  [ ("toPrettyString", "String -> JsonObject JsonValue -> String", "Convert a proper JSON object (i.e. JsonObject JsonValue) into a prettified string. The first argument is a separator token (e.g. \" \", \"\\t\", etc.) that will be used for indentation in the prettified string version of the JSON object.")
  , ("toPrettyJSString", "String -> JsonObject JsonValue -> JSString", "Same as toPrettyString except it produces a JavaScript string instead of an Elm string.")
  ]


categories =
  [ ("Using JSON Objects", objects)
  , ("List Conversions", lists)
  , ("Constructors for JsonValues", values)
  , ("String Conversions", strings)
  , ("Prettified Strings", prettyStrings)
  , ("JavaScript String Conversions", jsStrings)
  ]

main = createDocs "Foreign.JavaScript.JSON" categories