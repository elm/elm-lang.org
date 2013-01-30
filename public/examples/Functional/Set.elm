
{--  Set  --------------------------------------------------

A container that contains no duplicates. A set can hold
anything with a type in {String,Char,Int,Float,Time}.

More info at:
  http://elm-lang.org/docs/Data/Set.elm

-----------------------------------------------------------}

a = Set.fromList [1..5]
b = Set.fromList ([4..7] ++ [5..9])

main =
  flow down . map asText $
    [ a
    , b
    , Set.insert 42 a
    , Set.union a b
    , Set.intersect a b
    , Set.diff a b
    , Set.diff b a
    ]