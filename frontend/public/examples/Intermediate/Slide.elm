import Mouse
import Window

-- MODEL
data Update = Click (Float,Float) | TimeDelta Time

floatify (x,y) = (toFloat x, toFloat y)
input = let clickPos = floatify <~ sampleOn Mouse.clicks Mouse.position
        in  merge (Click <~ clickPos)
                  (TimeDelta <~ (40 `fpsWhen` (second `since` clickPos)))

-- UPDATE
step inp ((tx,ty),(x,y)) =
    case inp of
      Click t  -> (t, (x,y))
      TimeDelta d -> ((tx,ty), ( x + (tx-x) * (d/100) ,
                                 y + (ty-y) * (d/100) ))

-- DISPLAY
grad = radial (0,0) 20 (7,-15) 50
       [(0, rgb  255 95 152), (0.75, rgb  255 1 136), (1, rgba 255 1 136 0)]

follower (w,h) (target,(x,y)) =
  layers [ collage w h [ circle 100 |> gradient grad
                                    |> move (x - toFloat w / 2, toFloat h / 2 - y) ]
         , plainText "Click anywhere and the circle will follow." ]

main = follower <~ Window.dimensions ~ foldp step ((0,0),(0,0)) input

