
import Mouse (clicks)
import Time (every)


-- Displays the approximate time of your click

main = lift asText $ sampleOn clicks (every $ 1/4)