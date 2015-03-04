import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Markdown
import Time exposing (..)
import Window


-- MODEL

areaW = 407
areaH = 301

type alias Model =
  { x:Float, y:Float, vx:Float, vy:Float, dir:String }


hero : Model
hero =
  { x=0, y=0, vx=0, vy=0, dir="south" }


-- UPDATE

update : (Time, { x:Int, y:Int }, Bool) -> Model -> Model
update (timeDelta, direction, isRunning) model =
  model
    |> newVelocity isRunning direction
    |> setDirection direction
    |> updatePosition timeDelta


newVelocity : Bool -> { x:Int, y:Int } -> Model -> Model
newVelocity isRunning {x,y} model =
  let scale = if isRunning then 2 else 1
      newVel n =
        if x == 0 || y == 0
          then scale * toFloat n
          else scale * toFloat n / sqrt 2
  in
      { model |
          vx <- newVel x,
          vy <- newVel y
      }


setDirection : { x:Int, y:Int } -> Model -> Model
setDirection {x,y} model =
  { model |
      dir <-
        if  | x > 0 -> "east"
            | x < 0 -> "west"
            | y < 0 -> "south"
            | y > 0 -> "north"
            | otherwise -> model.dir
  }


updatePosition : Time -> Model -> Model
updatePosition dt ({x,y,vx,vy} as model) =
  { model |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx),
      y <- clamp (-areaH/2) (areaH/2) (y + dt * vy)
  }


-- VIEW

view : (Int,Int) -> Model -> Element
view (w,h) {x,y,vx,vy,dir} =
  let verb = if vx == 0 && vy == 0 then "stand" else "walk"
      src = "/imgs/hero/" ++ verb ++ "/" ++ dir ++ ".gif"
  in
      container w h middle <|
      collage areaW areaH
        [ toForm (image areaW areaH "/imgs/desert.png")
        , toForm (image 22 28 src)
            |> move (x,y)
        , toForm (Markdown.toElement "Arrows to move<br/>Shift to run")
            |> move (70-areaW/2, 30-areaH/2)
        ]


-- SIGNALS

main : Varying Element
main =
  Varying.map2 view Window.dimensions (Signal.foldp update hero input)


output : Varying (Time, { x:Int, y:Int }, Bool)
output =
  Signal.sampleOn delta (Varying.map3 (,,) delta Keyboard.arrows Keyboard.shift)


delta : Varying Time
delta =
  Varying.map (\t -> t / 20) (fps 25)
