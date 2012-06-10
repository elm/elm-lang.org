
------ Create squares  ----

fibHelp a b n = if n <= 0 then a else fibHelp b (a+b) (n-1)
fib = fibHelp 0 1

fibSquare n = 
  let { fN = fib n
      ; len = fN * 15
      ; clr = rgb (mod n 3 / 3) (mod n 7 / 7) (mod n 5 / 5)
      }
  in  color clr . size len len . box 5 $ asText fN


----  Combine squares  ----

ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }

dirs = [ beside, above, flip beside, below ]

combine n tiles =
  let dir = ith (n `mod` List.length dirs) dirs in
    dir tiles (fibSquare n)
    

----  Put it all together  ----

main = List.foldl combine (fibSquare 1) [2..7]