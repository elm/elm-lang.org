
import Signal.Random
import Signal.Time (every)

main = lift asText (randomize 0 100 (every 1))