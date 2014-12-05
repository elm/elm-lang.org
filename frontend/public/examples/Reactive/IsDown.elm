import Graphics.Element (..)
import Mouse
import Signal
import Text (asText)


-- Mouse.isDown is true whenever the left mouse button
-- is pressed down and false otherwise.

main : Signal Element
main =
    Signal.map asText Mouse.isDown


-- Try clicking. The boolean value will update automatically.