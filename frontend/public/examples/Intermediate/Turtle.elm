-- Move the Turtle around with the arrow keys.
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Signal
import Time exposing (..)
import Window

-- MODEL
type alias Turtle = { x:Float, y:Float, angle:Float, velocity:Float }

turtle : Turtle
turtle = { x=0, y=0, angle=0, velocity=0 }


-- UPDATE
type alias Arrows = { x:Float, y:Float }

update : (Arrows,Time) -> Turtle -> Turtle
update (arrows,timeDelta) turtle =
  turtle
    |> swimStep timeDelta
    |> keysStep arrows


keysStep : Arrows -> Turtle -> Turtle
keysStep arrows turtle =
  { turtle |
      velocity <- 40 * arrows.y,
      angle <- turtle.angle - arrows.x / 20
  }


swimStep : Time -> Turtle -> Turtle
swimStep delta ({x,y,angle,velocity} as turtle) =
  { turtle |
      x <- x + delta * velocity * cos angle,
      y <- y + delta * velocity * sin angle
  }


-- VIEW
view : (Int,Int) -> Turtle -> Element
view (w,h) turtle =
  let turtlePic =
        toForm (image 96 96 "/turtle.gif")
          |> rotate turtle.angle
          |> move (turtle.x,turtle.y)
  in
      layers
        [ collage w h [turtlePic]
        , opacity 0.7 <| fittedImage w h "/water.gif"
        ]


-- SIGNALS
main =
  Signal.map2 view Window.dimensions (Signal.foldp update turtle input)

input =
  let floatify {x,y} = { x = toFloat x, y = toFloat y }
  in
      Signal.map2 (,) (Signal.map floatify Keyboard.arrows) delta
        |> Signal.sampleOn delta

delta =
  Signal.map inSeconds (fps 30)

-- Try switching out Keyboard.arrows for Keyboard.wasd to
-- try out different controls.