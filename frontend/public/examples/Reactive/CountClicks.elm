
import Mouse

main : Signal Element
main =
    lift asText (count Mouse.clicks)