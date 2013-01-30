
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

(gameWidth, gameHeight) = (600,400)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(paddleWidth, paddleHeight) = (10, 40)
paddleSpeed = 200
paddlePadding = 20
ballSize = 15
ballSpeed = 150

data State = Play | Pause

player x = { x = x, y = halfHeight, vx = 0, vy = 0, score = 0 }
defaultGame =
  { state = Pause
  , ball = { x = halfWidth, y = halfHeight, vx = ballSpeed, vy = ballSpeed }
  , playerL = player paddlePadding
  , playerR = player (gameWidth - paddlePadding) }


-- Updates

stepObj t obj = let {x,y,vx,vy} = obj in
                { obj | x <- x + vx*t, y <- y + vy*t }

between lo hi n = n >= lo && n <= hi
within ball paddle =
  (between (paddle.x - paddleWidth/2 - ballSize/2) (paddle.x + paddleWidth/2 + ballSize/2) ball.x) &&
  (between (paddle.y - paddleHeight/2 - ballSize/2) (paddle.y + paddleHeight/2 + ballSize/2) ball.y)

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepBall t ball p1 p2 =
 if not (between 0 gameWidth ball.x)
   then { ball | x <- halfWidth, y <- halfHeight }
   else let {x,y,vx,vy} = ball in
        stepObj t { ball | vx <- stepV vx (ball `within` p1) (ball `within` p2)
                         , vy <- stepV vy (y < ballSize/2) (y > gameHeight-ballSize/2) }

stepPlayer t dir points player =
  let moved = stepObj t { player | vy <- 0 - dir*paddleSpeed }
  in  { moved | y <- clamp (paddleHeight/2) (gameHeight - paddleHeight/2) moved.y
                , score <- player.score + points }

stepGame (Input t (KeyInput space dirL dirR)) game =
  let {state,ball,playerL,playerR} = game
      scoreL = if ball.x > gameWidth then 1 else 0
      scoreR = if ball.x < 0         then 1 else 0
  in  { game | state   <- if | space -> Play
                             | (scoreL == 1) || (scoreR == 1) -> Pause
                             | otherwise -> state
             , ball    <- if state == Pause then ball else stepBall t ball playerL playerR
             , playerL <- stepPlayer t dirL scoreL playerL
             , playerR <- stepPlayer t dirR scoreR playerR }

gameState = foldp stepGame defaultGame input


-- Display

backgroundColor = rgb 60 100 60
textColor = rgb 160 200 160
txt f = text . f . monospace . Text.color textColor . toText
msg = "SPACE to start, WS and &uarr;&darr; to move"
positionOf obj = (obj.x, obj.y)

drawBackground = filled backgroundColor (rect gameWidth gameHeight (halfWidth,halfHeight))
drawBall ball = filled white (oval ballSize ballSize (positionOf ball))
drawPlayer player = filled white (rect paddleWidth paddleHeight (positionOf player))
drawScores scores = toForm (halfWidth, toFloat (heightOf scores) / 2 + 10) scores
drawMessage state = toForm (halfWidth, gameHeight - 40) (if state == Play then spacer 1 1 else txt id msg)

display (w,h) {state,ball,playerL,playerR} =
  let scores = txt (Text.height 4) (show playerL.score ++ "  " ++ show playerR.score)
  in container w h middle $ collage (round gameWidth) (round gameHeight)
       [ drawBackground
       , drawBall ball
       , drawPlayer playerL
       , drawPlayer playerR
       , drawScores scores
       , drawMessage state
       ]

main = lift2 display Window.dimensions gameState
