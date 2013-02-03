
module Mario where

mario = { x = 0, y = 0, vx = 0, vy = 0, dir = "right" }

jumpStep isJump obj = if isJump && obj.y == 0 then { obj | vy <- 5 } else obj
gravityStep t obj = { obj | vy <- if obj.y > 0 then obj.vy - t/4 else obj.vy }
timeStep t obj = let {x,y,vx,vy} = obj in
                 { obj | x <- x + t * vx , y <- max 0 $ y + t * vy }
walkStep dir obj = { obj | vx <- dir, dir <- if | dir < 0   -> "left"
                                                | dir > 0   -> "right"
                                                | otherwise -> obj.dir }

step t d j = timeStep t . gravityStep t . jumpStep j . walkStep d


delta = lift (flip (/) 20) (fps 25)
leftRight = toFloat . .x <~ Keyboard.arrows
jump = (\{y} -> y > 0) <~ Keyboard.arrows
steps = sampleOn delta (lift3 step delta leftRight jump)

main  = lift2 render Window.dimensions (foldp ($) mario steps)

render (w,h) mario =
  let verb = if mario.y  >  0 then "jump" else
             if mario.vx /= 0 then "walk" else "stand"
      src  = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in  collage w h [ filled (rgb 174 238 238) $ rect w h (w `div` 2, h `div` 2)
                  , filled (rgb 74 163 41) $ rect w 50 (w `div` 2,h-25)
                  , toForm (mario.x, (h-63)-mario.y) (image 35 35 src) ]
