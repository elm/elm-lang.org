
{------------------------------------------------------------------

  This example uses both `insert` and `remove`.

  Insert replaces old values if the given key is already present.

  Remove does nothing if the given key is not present.

------------------------------------------------------------------}


import JSON

bruce = fromList
         [ ("first", JsonString "Bruce")
         , ("last" , JsonString "Wayne")
         , ("age"  , JsonNumber 29)
         , ("nick" , JsonString "Batman")
         ]

retiredBruce = remove "nick" (insert "age" (JsonNumber 52) bruce)

main = flow down 
         [ plainText "Bruce Wayne"
         , text . monospace . toText $ JSON.toString bruce
         , plainText "retires and becomes"
         , text . monospace . toText $ JSON.toString retiredBruce
         ]