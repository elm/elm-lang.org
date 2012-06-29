
import Signal.Mouse (clicks)
import Signal.Time (every)


-- Displays the approximate time of your click

main = lift asText $ sampleOn clicks (every $ 1/4)