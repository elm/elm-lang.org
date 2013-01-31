-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/games-in-elm/part-0/Making-Pong.html

-- Inputs

data KeyInput = KeyInput Bool Int Int
defaultKeyInput = KeyInput False 0 0
keyInput = lift3 KeyInput Keyboard.space
                          (lift .y Keyboard.wasd)
                          (lift .y Keyboard.arrows)

data Input = Input Time KeyInput
delta = lift inSeconds (fps 50)
input = sampleOn delta (lift2 Input delta keyInput)


-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (gameWidth/2, gameHeight/2)

data State = Play | Pause

player x = { x = x, y = halfHeight, vx = 0, vy = 0, score = 0 }
defaultGame =
  { state = Pause
  , ball = { x = halfWidth, y = halfHeight, vx = 200, vy = 200 }
  , playerL = player 20
  , playerR = player (gameWidth-20) }


-- Updates

stepObj t obj = let {x,y,vx,vy} = obj in
                { obj | x <- x + vx*t, y <- y + vy*t }

between lo hi n = n >= lo && n <= hi
within ball paddle =
  and [ between (paddle.x - 8 ) (paddle.x + 8 ) ball.x
      , between (paddle.y - 20) (paddle.y + 20) ball.y ]

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepBall t ball p1 p2 =
 if not (between 0 gameWidth ball.x)
   then { ball | x <- halfWidth, y <- halfHeight }
   else let {x,y,vx,vy} = ball in
        stepObj t { ball | vx <- stepV vx (ball `within` p1) (ball `within` p2)
                         , vy <- stepV vy (y < 7) (y > gameHeight-7) }

stepPlyr t dir points player =
  let player1 = stepObj  t { player | vy <- 0 - dir * 200 }
  in  { player1 | y <- clamp 20 (gameHeight-20) player1.y
                , score <- player.score + points }

stepGame (Input t (KeyInput space dirL dirR)) game =
  let {state,ball,playerL,playerR} = game
      scoreL = if ball.x > gameWidth then 1 else 0
      scoreR = if ball.x < 0         then 1 else 0
  in  {game| state   <- if | space            -> Play
                           | scoreL /= scoreR -> Pause
                           | otherwise        -> state
           , ball    <- if state == Pause then ball else
                            stepBall t ball playerL playerR
           , playerL <- stepPlyr t dirL scoreL playerL
           , playerR <- stepPlyr t dirR scoreR playerR }

gameState = foldp stepGame defaultGame input


-- Display

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText
msg = "SPACE to start, WS and &uarr;&darr; to move"
positionOf obj = (obj.x, obj.y)

display (w,h) {state,ball,playerL,playerR} =
  let scores = txt (Text.height 4) (show playerL.score ++ "  " ++ show playerR.score)
  in container w h middle $ collage (round gameWidth) (round gameHeight)
       [ filled pongGreen (rect gameWidth gameHeight (halfWidth,halfHeight))
       , filled white (oval 15 15 (positionOf ball))
       , filled white (rect 10 40 (positionOf playerL))
       , filled white (rect 10 40 (positionOf playerR))
       , toForm (halfWidth, toFloat (heightOf scores) / 2 + 10) scores
       , toForm (halfWidth, gameHeight - 40)
           (if state == Play then spacer 1 1 else txt id msg)
       ]

main = lift2 display Window.dimensions gameState
