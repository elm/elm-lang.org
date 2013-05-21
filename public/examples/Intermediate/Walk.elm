
import Keyboard
import Window

-- MODEL
areaW = 407
areaH = 301

obj = { x=200, y=150, vx=0, vy=0, verb="stand", dir="south" }


-- UPDATE
velStep d obj =
  let f n = if d.x == 0 || d.y == 0 then n else n / sqrt 2
  in  { obj | vx <- f d.x, vy <- f (0-d.y) }

verbStep d obj =
  let {vx,vy} = obj in
  { obj | verb <- if vx == 0 && vy == 0 then "stand" else "walk"
        , dir  <- if | vx > 0 -> "east"
                     | vx < 0 -> "west"
                     | vy < 0 -> "north"
                     | vy > 0 -> "south"
                     | otherwise -> obj.dir }

runStep b obj =
  let scale = if b then 1.5 else 1
  in  { obj | vx <- obj.vx * scale, vy <- obj.vy * scale }

timeStep t obj = let {x,y,vx,vy} = obj
                 in  { obj | x <- clamp 0 areaW (x + t * vx) ,
                             y <- clamp 0 areaH (y + t * vy) }

step time arrows run =
  timeStep time . verbStep arrows . runStep run . velStep arrows


-- LINK
delta = lift (flip (/) 20) (fps 25)
steps = sampleOn delta (lift3 step delta Keyboard.arrows Keyboard.ctrl)

main  = lift2 display Window.dimensions (foldp ($) obj steps)


-- DISPLAY
display (w,h) {x,y,verb,dir} =
  container w h middle $ flow down
    [ layers [ image areaW areaH "/imgs/desert.png"
             , let src = "/imgs/hero/" ++ verb ++ "/" ++ dir ++ ".gif"
                   pos = middleAt (absolute x) (absolute y)
               in  container areaW areaH pos (image 22 28 src)
             ]
    , [markdown|Move with arrows, run with ctrl.|]
    ]
