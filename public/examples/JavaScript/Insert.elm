
{------------------------------------------------------------------

  Insert a value into a JsonObject. Replaces old value if key is
  already present.

------------------------------------------------------------------}


import Foreign.JavaScript.JSON


bruce = fromList
         [ ("first", JsonString "Bruce")
         , ("last" , JsonString "Wayne")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonString "Batman")
         ]

retiredBruce = remove "nick" (insert "age" (JsonNumber 52) bruce)


"age" bruce