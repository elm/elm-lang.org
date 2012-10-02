
{------------------------------------------------------------------
                 Lookup a field in a JsonObject.
------------------------------------------------------------------}


import Foreign.JavaScript.JSON


bruce = fromList
         [ ("first", JsonString "Bruce")
         , ("last" , JsonString "Wayne")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonString "Batman")
         ]

main = asText $ lookup "age" bruce