
-- The following two functions will be library functions.

unzip ps =
  case ps of
  { (x,y):rest -> case unzip rest of { (xs,ys) -> (x:xs, y:ys) }
  ; [] -> ([],[])
  }

scatterPlot points =
  let w = 400, h = 400 in
  case unzip points of { (xs,ys) ->
    canvas w h . map (outlined blue . ngon 4 3) $
    zip (map (\x -> x * w / (maximum xs * (11/10))) xs)
        (map (\y -> h - y * h / (maximum ys * (11/10))) ys)
  }

-- To create a scatter plot, only the following will be
-- necessary:


main = scatterPlot [(1,2), (2,4), (3,7), (4,9), (5,8), (6,10), (7,5)]