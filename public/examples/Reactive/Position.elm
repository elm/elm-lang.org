
-- Display the current position of the mouse.

import Signal.Mouse (position)

main = lift asText position