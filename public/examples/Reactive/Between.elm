
-- Demonstrates Time.before and Time.after, booolean signals
-- indicating whether or not a given amount of time has passed.

import Time (before,after)

between t1 t2 = lift2 (&&) (after t1) (before t2)

main = lift msg (between 3 6)



msg isBetween = text $
  fromString "This page opened between 3 and 6 seconds ago: " ++
  (if isBetween then Text.color green (fromString "Yes!")
                else Text.color red (fromString "No..."))