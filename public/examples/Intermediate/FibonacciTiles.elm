
------ Create squares  ----

fibHelp a b n = if n <= 0 then a else fibHelp b (a+b) (n-1)
fib = fibHelp 0 1

fibSquare n = 
  let { fN = fib n
      ; len = fN * 15
      ; clr = rgb ((85*n) `mod` 256) ((36*n) `mod` 256) ((51*n) `mod` 256)
      }
  in  color clr . container len len middle $ asText fN


----  Combine squares  ----

ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }

dirs = [ beside, above, flip beside, below ]

combine n tiles =
  let dir = ith (n `mod` length dirs) dirs in
    dir tiles (fibSquare n)
    

----  Put it all together  ----

main = foldl combine (fibSquare 1) [2..7]