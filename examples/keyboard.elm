-- Move a square around with the arrow keys: UP, DOWN, LEFT, RIGHT
-- Try making it move around more quickly!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Playground exposing (..)


main =
  game view update (0,0)


view computer (x,y) =
  [ square blue 40
      |> move x y
  ]


update computer (x,y) =
  ( x + toX computer.keyboard
  , y + toY computer.keyboard
  )
