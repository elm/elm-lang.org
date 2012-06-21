
{----------------------------------------------------------------

Overview:
  zipWith is just like zip except you can tell it how to zip the
  two lists together. It takes one extra argument, a function f
  that is applied pairwise to the elements of each list.

Fun Fact: We can even define zip in terms of zipWith!
    zip = zipWith (\x y -> (x,y))

----------------------------------------------------------------}

import Data.List (zipWith)

main = asText $ zipWith (:) ['c','m','t'] ["hop","ill","ape"]
