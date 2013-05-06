
import Either
import Mouse
import Window

input = let clickPos = sampleOn Mouse.clicks Mouse.position
        in  mergeEither clickPos (40 `fpsWhen` (second `since` clickPos))

step inp ((tx,ty),(x,y)) =
    case inp of
      Left t  -> (t, (x,y))
      Right d -> ((tx,ty), ( x + (tx-x) * (d/100) ,
                             y + (ty-y) * (d/100) ))

follower (w,h) (target,(x,y)) =
  layers [ collage w h [ circle 16 |> filled cyan
                                   |> move (x- toFloat w/2) (toFloat h/2-y) ]
         , plainText "Click anywhere and the circle will follow." ]

main = lift2 follower Window.dimensions
                      (foldp step ((0,0),(0,0)) input)

