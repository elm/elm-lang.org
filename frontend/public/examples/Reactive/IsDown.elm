import Graphics.Element exposing (..)
import Mouse
import Text exposing (asText)


-- Mouse.isDown is true whenever the left mouse button
-- is pressed down and false otherwise.

main : Varying Element
main =
    Varying.map asText Mouse.isDown


-- Try clicking. The boolean value will update automatically.