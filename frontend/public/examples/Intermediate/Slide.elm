import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Time exposing (..)
import Window


-- MODEL
type Update = Click (Float,Float) | TimeDelta Time

-- UPDATE
update inp ((tx,ty),(x,y)) =
    case inp of
      Click t  -> (t, (x,y))
      TimeDelta d -> ((tx,ty), ( x + (tx-x) * (d/100) ,
                                 y + (ty-y) * (d/100) ))

-- DISPLAY
view (w,h) (target,(x,y)) =
  layers
    [ collage w h
        [ circle 100
            |> gradient pinkGradient
            |> move (x - toFloat w / 2, toFloat h / 2 - y)
        ]
    , show "Click anywhere and the circle will follow."
    ]

pinkGradient =
  radial (0,0) 20 (7,-15) 50
   [(0, rgb  255 95 152), (0.75, rgb  255 1 136), (1, rgba 255 1 136 0)]


-- SIGNALS
main =
  Signal.map2 view Window.dimensions (Signal.foldp update ((0,0),(0,0)) input)

input =
  let floatify (x,y) = (toFloat x, toFloat y)
      clickPos = Signal.map floatify (Signal.sampleOn Mouse.clicks Mouse.position)
  in
      Signal.merge
        (Signal.map Click clickPos)
        (Signal.map TimeDelta (40 `fpsWhen` (second `since` clickPos)))

