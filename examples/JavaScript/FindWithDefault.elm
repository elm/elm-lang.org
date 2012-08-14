
{------------------------------------------------------------------

  Find a value in a JsonObject, providing a default value in case
  nothing is found for the given field name.

------------------------------------------------------------------}


import Foreign.JavaScript.JSON


bruce = fromList
         [ ("first", JsonString "Bruce")
         , ("last" , JsonString "Wayne")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonString "Batman")
         ]

main = asText $ findWithDefault JsonNull "age" bruce