
import Array (Array)
import Array

dimensions : Int
dimensions = 15

matrix : Array (Array Int)
matrix =
  let row n = Array.initialize dimensions (\i -> dimensions * n + i)
  in  Array.initialize dimensions row

main : Element
main =
  let rows = Array.toList matrix

      box n = let clr = hsv (degrees (toFloat n)) 0.9 0.9
              in  color clr (spacer 10 10)

      colors row = flow right (map box (Array.toList row))
  in
      flow down (map colors rows)