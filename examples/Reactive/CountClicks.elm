
-- This counts the number of time you have clicked.


import Signal.Mouse (clicks)

main = lift asText (count clicks)