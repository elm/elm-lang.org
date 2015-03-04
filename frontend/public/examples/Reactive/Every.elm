import Graphics.Element exposing (..)
import Text exposing (asText)
import Time exposing (every, second)


main : Varying Element
main =
  Varying.map asText (every second)

-- For a much more exciting way to tell time, see
-- http://elm-lang.org/edit/examples/Intermediate/Clock.elm
