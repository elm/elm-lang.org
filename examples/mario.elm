-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


import Playground exposing (..)



-- MAIN


main =
  game view update
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = "right"
    }


-- VIEW


view computer mario =
  let
    w = computer.screen.width
    h = computer.screen.height
    b = computer.screen.bottom
  in
  [ rectangle (rgb 174 238 238) w h
  , rectangle (rgb 74 163 41) w 100
      |> moveY b
  , image 70 70 (toGif mario)
      |> move mario.x (b + 76 + mario.y)
  ]


toGif mario =
  if mario.y > 0 then
    "https://elm-lang.org/images/mario/jump/" ++ mario.dir ++ ".gif"
  else if mario.vx /= 0 then
    "https://elm-lang.org/images/mario/walk/" ++ mario.dir ++ ".gif"
  else
    "https://elm-lang.org/images/mario/stand/" ++ mario.dir ++ ".gif"



-- UPDATE


update computer mario =
  let
    dt = 1.666
    vx = toX computer.keyboard
    vy =
      if mario.y == 0 then
        if computer.keyboard.up then 5 else 0
      else
        mario.vy - dt / 8
    x = mario.x + dt * vx
    y = mario.y + dt * vy
  in
  { x = x
  , y = max 0 y
  , vx = vx
  , vy = vy
  , dir = if vx == 0 then mario.dir else if vx < 0 then "left" else "right"
  }
