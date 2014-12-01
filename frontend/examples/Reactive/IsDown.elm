
import Mouse

-- Mouse.isDown is true whenever the left mouse button
-- is pressed down and false otherwise.

main : Signal Element
main =
    lift asText Mouse.isDown


-- Try clicking. The boolean value will update automatically.