
-- Displays the position of the latest click.

import Mouse

main : Signal Element
main =
    lift asText (sampleOn Mouse.clicks Mouse.position)