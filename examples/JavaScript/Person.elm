
{------------------------------------------------------------------

  Creates a JsonObject that represents a person.

  Notice that the association list is a mapping from Strings to
  JsonValues. Values must be wrapped in constructors (such as
  JsonString and JsonNumber) so that they all have the same type.

------------------------------------------------------------------}


import Foreign.JavaScript.JSON

john = fromList
         [ ("first", JsonString "John")
         , ("last" , JsonString "Doe")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonNull)
         ]

main = text . monospace . toText $ toString john