-- Try changing the value passed to 'pascals' in the triangle function.
-- Don't go crazy though; your browser can only do so much!

import Window as Window

step row = zipWith (+) (0 :: row) (row ++ [0])
pascals depth = scanl (\_ -> step) [1] [1..depth-1]

triangle w =
    let style = width w . centered . monospace . toText . show
    in  flow down . map style <| pascals 8

main = lift triangle Window.width