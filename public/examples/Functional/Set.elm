{--  Set  --------------------------------------------------

A container that contains no duplicates. A set can hold
anything with a type in {String,Char,Int,Float,Time}.

More info at:
  http://elm-lang.org/docs/Data/Set.elm

-----------------------------------------------------------}

import Set

xs = Set.fromList [1..5]
ys = Set.fromList ([4..7] ++ [5..9])

main =
  flow down . List.map asText <|
    [ xs,
      ys,
      Set.insert 42 xs,
      Set.union xs ys,
      Set.intersect xs ys,
      Set.diff xs ys,
      Set.diff ys xs ]