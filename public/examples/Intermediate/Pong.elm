-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/Pong.elm

import Keyboard
import Text
import Window

-- Inputs

type Input = { space:Bool, dir1:Int, dir2:Int, delta:Time }

delta = inSeconds <~ fps 35

input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .y Keyboard.wasd
                               ~ lift .y Keyboard.arrows
                               ~ delta)


-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

data State = Play | Pause

type Ball = { x:Float, y:Float, vx:Float, vy:Float }
type Player = { x:Float, y:Float, vx:Float, vy:Float, score:Int }
type Game = { state:State, ball:Ball, player1:Player, player2:Player }

player : Float -> Player
player x = { x=x, y=0, vx=0, vy=0, score=0 }

defaultGame : Game
defaultGame =
  { state   = Pause,
    ball    = { x=0, y=0, vx=200, vy=200 },
    player1 = player (20-halfWidth) ,
    player2 = player (halfWidth-20) }


-- Updates

stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

near k c n = n >= k-c && n <= k+c
within ball paddle = (ball.x |> near paddle.x 8)
                  && (ball.y |> near paddle.y 20)

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({x,y,vx,vy} as ball) p1 p2 =
  if not (ball.x |> near 0 halfWidth)
  then { ball | x <- 0, y <- 0 }
  else stepObj t { ball | vx <- stepV vx (ball `within` p1) (ball `within` p2) ,
                          vy <- stepV vy (y < 7-halfHeight) (y > halfHeight-7) }

stepPlyr : Time -> Int -> Int -> Player -> Player
stepPlyr t dir points player =
  let player1 = stepObj  t { player | vy <- toFloat dir * 200 }
  in  { player1 | y <- clamp (22-halfHeight) (halfHeight-22) player1.y
                , score <- player.score + points }

stepGame : Input -> Game -> Game
stepGame {space,dir1,dir2,delta} ({state,ball,player1,player2} as game) =
  let score1 = if ball.x >  halfWidth then 1 else 0
      score2 = if ball.x < -halfWidth then 1 else 0
  in  {game| state   <- if | space            -> Play
                           | score1 /= score2 -> Pause
                           | otherwise        -> state
           , ball    <- if state == Pause then ball else
                            stepBall delta ball player1 player2
           , player1 <- stepPlyr delta dir1 score1 player1
           , player2 <- stepPlyr delta dir2 score2 player2 }

gameState = foldp stepGame defaultGame input


-- Display

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = leftAligned . f . monospace . Text.color textGreen . toText
msg = "SPACE to start, WS and &uarr;&darr; to move"
make obj shape =
    shape |> filled white
          |> move (obj.x,obj.y)

display : (Int,Int) -> Game -> Element
display (w,h) {state,ball,player1,player2} =
  let scores : Element
      scores = txt (Text.height 50) (show player1.score ++ "  " ++ show player2.score)
  in container w h middle <| collage gameWidth gameHeight
       [ rect gameWidth gameHeight |> filled pongGreen
       , oval 15 15 |> make ball
       , rect 10 40 |> make player1
       , rect 10 40 |> make player2
       , toForm scores |> move (0, gameHeight/2 - 40)
       , toForm (if state == Play then spacer 1 1 else txt identity msg)
           |> move (0, 40 - gameHeight/2)
       ]

main = lift2 display Window.dimensions gameState
