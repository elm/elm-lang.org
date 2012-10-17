
import Mouse (isDown)


-- Mouse.isDown is true whenever the left mouse button
-- is pressed down and false otherwise.

main = lift asText isDown


-- Try clicking. The boolean value will update automatically.