
{------------------------------------------------------------------

  Creates a JsonObject that represents a list of people.

  The `JsonArray` constructor must take a list of JsonValues.
  `john` and `bruce` both have type (JsonObjects JsonValue).
  We apply the `JsonObject` constructor to make them JsonValues.

------------------------------------------------------------------}


import Foreign.JavaScript.JSON


-- john :: JsonObject JsonValue

john = fromList
         [ ("first", JsonString "John")
         , ("last" , JsonString "Doe")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonNull)
         ]


-- bruce :: JsonObject JsonValue

bruce = fromList
         [ ("first", JsonString "Bruce")
         , ("last" , JsonString "Wayne")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonString "Batman")
         ]


-- people :: JsonObject JsonValue

people = singleton "people" (JsonArray $ map JsonObject [ john, bruce ])


main = text . monospace . toText $ toString people