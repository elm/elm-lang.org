
factorial n = if n <= 1 then 1 else n * factorial (n-1)

main = asText (factorial 5)