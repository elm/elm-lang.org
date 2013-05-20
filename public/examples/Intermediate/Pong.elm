-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/games-in-elm/part-0/Making-Pong.html

import Keyboard
import Window

-- Inputs

type Input = { space:Bool, dirL:Int, dirR:Int, delta:Time }

delta = inSeconds <~ fps 40

input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .y Keyboard.wasd
                               ~ lift .y Keyboard.arrows
                               ~ delta)


-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

data State = Play | Pause

player x = { x=x, y=0, vx=0, vy=0, score=0 }
defaultGame =
  { state   = Pause,
    ball    = { x=0, y=0, vx=200, vy=200 },
    playerL = player (20-halfWidth) ,
    playerR = player (halfWidth-20) }


-- Updates

stepObj t obj = let {x,y,vx,vy} = obj in
                { obj | x <- x + vx*t, y <- y + vy*t }

near k c n = n >= k-c && n <= k+c
within ball paddle = (ball.x |> near paddle.x 8)
                  && (ball.y |> near paddle.y 20)

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepBall t ball p1 p2 =
  if not (ball.x |> near 0 halfWidth)
  then { ball | x <- 0, y <- 0 }
  else let {x,y,vx,vy} = ball in
       stepObj t { ball | vx <- stepV vx (ball `within` p1) (ball `within` p2) ,
                          vy <- stepV vy (y < 7-halfHeight) (y > halfHeight-7) }

stepPlyr t dir points player =
  let player1 = stepObj  t { player | vy <- dir * 200 }
  in  { player1 | y <- clamp (22-halfHeight) (halfHeight-22) player1.y
                , score <- player.score + points }

stepGame {space,dirL,dirR,delta} game =
  let {state,ball,playerL,playerR} = game
      scoreL = if ball.x >   halfWidth then 1 else 0
      scoreR = if ball.x < 0-halfWidth then 1 else 0
  in  {game| state   <- if | space            -> Play
                           | scoreL /= scoreR -> Pause
                           | otherwise        -> state
           , ball    <- if state == Pause then ball else
                            stepBall delta ball playerL playerR
           , playerL <- stepPlyr delta dirL scoreL playerL
           , playerR <- stepPlyr delta dirR scoreR playerR }

gameState = foldp stepGame defaultGame input


-- Display

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText
msg = "SPACE to start, WS and &uarr;&darr; to move"
make obj shape = shape |> filled white
                       |> move (obj.x,obj.y)

display (w,h) {state,ball,playerL,playerR} =
  let scores = txt (Text.height 4) (show playerL.score ++ "  " ++ show playerR.score)
  in container w h middle $ collage (round gameWidth) (round gameHeight)
       [ rect gameWidth gameHeight |> filled pongGreen
       , oval 15 15 |> make ball
       , rect 10 40 |> make playerL
       , rect 10 40 |> make playerR
       , toForm scores |> move (0, gameHeight/2 - 40)
       , toForm (if state == Play then spacer 1 1 else txt id msg)
           |> move (0, 40 - gameHeight/2)
       ]

main = lift2 display Window.dimensions gameState
