
import Window

main = lift triangle Window.width

triangle w =
    let style = show >> toText >> monospace >> centered >> width w
    in  flow down <| map style (pascals 8)

-- Try changing the value passed to 'pascals' above.


step row = zipWith (+) (0 :: row) (row ++ [0])
pascals depth = scanl (\_ -> step) [1] [1..depth-1]
