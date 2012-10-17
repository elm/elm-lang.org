
-- This counts the number of time you have clicked.


import Mouse (clicks)

main = lift asText (count clicks)