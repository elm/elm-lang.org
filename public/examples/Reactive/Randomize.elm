
import Random

main : Signal Element
main =
    lift asText (Random.range 0 100 (every second))