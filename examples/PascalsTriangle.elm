
-- Try changing the value passed to 'pascals' in the last line.
-- Don't go crazy though; your browser can only do so much!

step row = zipWith (+) (0 : row) (row ++ [0])
pascals depth = scanl (\x -> step) [1] [1..depth-1]

main = flowDown . map (centerText . show) $ pascals 5