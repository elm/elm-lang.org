import Graphics.Element exposing (..)
import Signal
import Text exposing (asText)
import Time exposing (every, second)


main : Signal Element
main =
  Signal.map asText (every second)

-- For a much more exciting way to tell time, see
-- http://elm-lang.org/edit/examples/Intermediate/Clock.elm
