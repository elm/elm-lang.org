
import Data.List (zipWith)


step row = zipWith (+) (0 : row) (row ++ [0])
pascals depth = scanl (\_ -> step) [1] [1..depth-1]

triangle w = flow down . map (width w . centeredText . show) $ pascals 5
main = lift triangle Window.width


-- Try changing the value passed to 'pascals' in the triangle function.
-- Don't go crazy though; your browser can only do so much!
