import Keyboard
import Window

-- MODEL
areaW = 407
areaH = 301

hero : { x:Float, y:Float, vx:Float, vy:Float, dir:String }
hero = { x=0, y=0, vx=0, vy=0, dir="south" }


-- UPDATE
velStep d obj =
    let f n = if d.x == 0 || d.y == 0 then toFloat n else toFloat n / sqrt 2
    in  { obj | vx <- f d.x, vy <- f d.y }

dirStep {x,y} obj =
    { obj | dir <- if | x > 0 -> "east"
                      | x < 0 -> "west"
                      | y < 0 -> "south"
                      | y > 0 -> "north"
                      | otherwise -> obj.dir }

runStep running obj =
    let scale = if running then 2 else 1
    in  { obj | vx <- obj.vx * scale, vy <- obj.vy * scale }

timeStep t ({x,y,vx,vy} as obj) =
    { obj | x <- clamp (-areaW/2) (areaW/2) (x + t * vx) ,
            y <- clamp (-areaH/2) (areaH/2) (y + t * vy) }

step (time,arrows,run) hero =
    timeStep time . dirStep arrows . runStep run . velStep arrows <| hero


-- HERO
delta = lift (\t -> t / 20) (fps 25)
input = sampleOn delta (lift3 (,,) delta Keyboard.arrows Keyboard.ctrl)

main  = lift2 display Window.dimensions (foldp step hero input)


-- DISPLAY
display (w,h) {x,y,vx,vy,dir} =
  container w h middle . collage areaW areaH <|
    [ toForm (image areaW areaH "/imgs/desert.png")
    , let verb = if vx == 0 && vy == 0 then "stand" else "walk"
          src = "/imgs/hero/" ++ verb ++ "/" ++ dir ++ ".gif"
      in  move (x,y) (toForm (image 22 28 src))
    , toForm [markdown|Arrows to move<br/>Ctrl to run|]
        |> move (70-areaW/2, 30-areaH/2)
    ]
