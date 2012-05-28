
fibHelp a b n = if n <= 0 then a else fibHelp b (a+b) (n-1)
fib = fibHelp 0 1

-- The third element of this list should simply read 'beside'.
-- I am working on it!
directions = [ flip beside, below
             , \a b -> height (getHeight b) $ beside a b, above ]

fibSquare n = 
  let fN = fib n
    , len = fN * 15
    , clr = rgb (mod n 3 / 3) (mod n 7 / 7) (mod n 5 / 5)
  in  color clr . size len len . height 14 . centerText $ show fN

combine n tiles = let dir = ith (mod n $ length directions) directions in
                  dir tiles (fibSquare n)

main = foldl combine (fibSquare 1) [2..7]