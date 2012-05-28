

-- This counts the number of time you have clicked.

clickCount = foldp (\b count -> if b then count + 1 else count) 0 Mouse.isClicked

main = lift asText clickCount