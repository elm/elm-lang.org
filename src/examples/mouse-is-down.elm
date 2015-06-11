import Graphics.Element exposing (..)
import Mouse


-- Mouse.isDown is true whenever the left mouse button
-- is pressed down and false otherwise.

main : Signal Element
main =
  Signal.map show Mouse.isDown


-- Try clicking. The boolean value will update automatically.