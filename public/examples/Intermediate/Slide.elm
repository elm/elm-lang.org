
import Either
import Mouse
import Window

data Update = Click (Int,Int) | TimeDelta Time

input = let clickPos = sampleOn Mouse.clicks Mouse.position
        in  merge (Click <~ clickPos)
                  (TimeDelta <~ (40 `fpsWhen` (second `since` clickPos)))

step inp ((tx,ty),(x,y)) =
    case inp of
      Click t  -> (t, (x,y))
      TimeDelta d -> ((tx,ty), ( x + (tx-x) * (d/100) ,
                                 y + (ty-y) * (d/100) ))

follower (w,h) (target,(x,y)) =
  layers [ collage w h [ circle 16 |> filled cyan
                                   |> move (x- toFloat w/2, toFloat h/2-y) ]
         , plainText "Click anywhere and the circle will follow." ]

main = lift2 follower Window.dimensions
                      (foldp step ((0,0),(0,0)) input)

