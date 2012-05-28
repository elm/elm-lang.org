
data Person = Person { first :: String, last :: String, age :: Int }

people = [ { first = "Evan", last = "Czaplicki", age = 21 }
         , { first = "John", last = "Doe", age = 19 }
         , { first = "Alice", last = "Smith", age = 25 }
         ]

display person =
    text $ person.first ++ " " ++ person.last ++ " : " ++ show person.age

options = [ ("First Name", sortBy (\a b -> compare a.first b.first))
          , ("Last Name" , sortBy (\a b -> compare a.last b.last))
          , ("Age"       , sortBy (\a b -> compare a.age b.age))
          ]

main = let (selector, personSort) = select options in
       flowDown [ flowRight [ text "Sort by: ", selector ]
                , flowDown . map display $ personSort people
                ]