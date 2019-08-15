-- Use arrow keys to move the turtle around.
--
-- Forward with UP and turn with LEFT and RIGHT.
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Playground exposing (..)


main =
  game view update
    { x = 0
    , y = 0
    , angle = 0
    }


view computer turtle =
  [ rectangle blue computer.screen.width computer.screen.height
  , image 96 96 "/images/turtle.gif"
      |> move turtle.x turtle.y
      |> rotate turtle.angle
  ]


update computer turtle =
  { x = turtle.x + toY computer.keyboard * cos (degrees turtle.angle)
  , y = turtle.y + toY computer.keyboard * sin (degrees turtle.angle)
  , angle = turtle.angle - toX computer.keyboard
  }
