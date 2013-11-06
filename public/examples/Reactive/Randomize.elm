
import Random

main = lift asText (Random.range 0 100 (every second))