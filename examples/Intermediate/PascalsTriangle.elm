
-- Try changing the value passed to 'pascals' in the last line.
-- Don't go crazy though; your browser can only do so much!

step row = List.zipWith (+) (0 : row) (row ++ [0])
pascals depth = List.scanl (\x -> step) [1] [1..depth-1]

triangle w = flow down . List.map (width w . centeredText . show) $ pascals 5
main = lift triangle Window.width