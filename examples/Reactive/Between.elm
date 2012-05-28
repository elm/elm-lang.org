
-- Demonstrates Time.before and Time.after, booolean signals
-- indicating whether or not a given amount of time has passed.

between t1 t2 = lift2 (&&) (Time.after t1) (Time.before t2)

main = lift msg (between 3 6)



msg isBetween = text $
  fromString "This page opened between 3 and 6 seconds ago: " ++
  (if isBetween then Text.color green (fromString "Yes!")
                else Text.color red (fromString "No..."))