
-- Displays the position of the latest click.

import Mouse

main = lift asText (sampleOn Mouse.clicks Mouse.position)