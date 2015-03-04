import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


main : Varying Element
main =
  Stream.map inSeconds (fps 30)
    |> Stream.fold step { height = 0, velocity = bounceVelocity }
    |> Varying.map view


-- Draw the sky, ball, and ground
view ball =
  collage 150 400
    [ rect 150 400
        |> filled (rgb 135 206 250)
    , circle 15
        |> filled red
        |> move (0, ball.height - 160)
    , rect 150 50
        |> filled green
        |> move (0,-200)
    ]


-- Physics
gravity = 140
bounceVelocity = 250

step time ball =
  { ball |
      height <-
          ball.height + ball.velocity * time
  ,
     velocity <-
        if ball.height < 0
          then bounceVelocity
          else ball.velocity - gravity * time
  }
