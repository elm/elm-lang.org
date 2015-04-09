import Graphics.Element exposing (..)
import Time exposing (every, second)


main : Signal Element
main =
  Signal.map show (every second)

-- For a much more exciting way to tell time, see
-- http://elm-lang.org/edit/examples/Intermediate/Clock.elm
