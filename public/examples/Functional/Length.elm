
length xs = case xs of { [] -> 0; _ : rest -> 1 + length rest }

main = asText (length [1..9])