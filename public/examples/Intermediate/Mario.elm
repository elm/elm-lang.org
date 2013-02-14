
-- Model
mario = { x = 0, y = 0, vx = 0, vy = 0, dir = "right" }

-- Update for Keyboard Input
jumpStep {y} obj = if y > 0 && obj.y == 0 then { obj | vy <- 5 } else obj
walkStep {x} obj = { obj | vx <- x, dir <- if | x < 0     -> "left"
                                              | x > 0     -> "right"
                                              | otherwise -> obj.dir }

-- Update for Time Input
gravityStep t obj = { obj | vy <- if obj.y > 0 then obj.vy - t/4 else obj.vy }
timeStep t obj = let {x,y,vx,vy} = obj in
                 { obj | x <- x + t*vx , y <- max 0 (y + t*vy) }

-- Update
step (t,dir) = timeStep t . gravityStep t . jumpStep dir . walkStep dir

-- Inputs
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

-- Display
render (w,h) mario =
  let verb = if mario.y  >  0 then "jump" else
             if mario.vx /= 0 then "walk" else "stand"
      src  = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w h
       [ filled (rgb 174 238 238) $ rect w h (w `div` 2, h `div` 2)
       , filled (rgb 74 163 41) $ rect w 50 (w `div` 2,h-25)
       , toForm (mario.x, (h-63)-mario.y) (image 35 35 src) ]

-- Putting it all together
main  = lift2 render Window.dimensions (foldp step mario input)
