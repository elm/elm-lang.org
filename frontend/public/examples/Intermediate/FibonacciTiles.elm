import Color exposing (rgb)
import Graphics.Element exposing (..)
import List
import Text exposing (asText)


------ Create squares  ----

fib : Int -> Int
fib n = fibHelp 0 1 n

fibHelp a b n =
  if n <= 0 then a else fibHelp b (a+b) (n-1)


fibSquare : Int -> Element
fibSquare n = 
  let fN = fib n
      len = fN * 15
      clr = rgb ((85*n) % 256) ((36*n) % 256) ((51*n) % 256)
  in
      color clr <| container len len middle (asText fN)


----  Combine squares  ----

ith i lst =
  case lst of
    x::xs -> if i == 0 then x else ith (i-1) xs

dirs = [ beside, above, flip beside, below ]

combine n tiles =
  let dir = ith (n % List.length dirs) dirs
  in
      dir tiles (fibSquare n)
    

----  Put it all together  ----

main : Element
main =
  List.foldl combine (fibSquare 1) [2..7]