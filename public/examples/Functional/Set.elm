-- A set is a container that has no duplicates.

import Set

xs = Set.fromList [1..5]
ys = Set.fromList [4,4,5,5,4,6,7,8]

main = flow down <| map asText
       [ xs
       , ys
       , Set.insert 42 xs
       , Set.remove 1 xs
       , Set.union xs ys
       , Set.intersect xs ys
       , Set.diff xs ys
       , Set.diff ys xs
       ]