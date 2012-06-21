
import Data.List (zip,unzip)
import Signal.Input (dropDown)

----  Create graphs from scratch  ----

data Style = Points | Line

plot style w h points =
  case unzip points of { (xs,ys) ->
    let { e = 26/25
        ; xmin = e * minimum xs ; xmax = e * maximum xs
        ; ymin = e * minimum ys ; ymax = e * maximum ys
        ; fit scale lo hi z = scale * abs (z-lo) / abs (hi-lo)
        ; f (x,y) = (fit w xmin xmax x, h - fit h ymin ymax y)
        ; axis a b = solid black . line . map f $ [a,b]
        ; xaxis = axis (xmin, clamp ymin ymax 0) (xmax, clamp ymin ymax 0)
        ; yaxis = axis (clamp xmin xmax 0, ymin) (clamp xmin xmax 0, ymax)
        ; draw ps = case style of
                    { Points -> map (outlined blue . ngon 4 3) ps
                    ; Line   -> [ solid blue $ line ps ]
                    }
        }
    in  collage w h $ [ xaxis, yaxis ] ++ draw (map f points)
  }


-----  Provide many graphs for display  ----

range = [ 0-10 .. 10 ]
piRange = map (\x -> x / 20 * pi) [0-20..20]
offRange = map (\x -> x/5) [0-20..10]

graph f range = zip range (map f range)

styles = [ ("Line Graph", Line)
         , ("Scatter Plot", Points)
         ]

points = [ ("Circle"     , map (\t -> (cos t, sin t)) piRange)
         , ("x^2"        , graph (\x -> x*x) range)
         , ("x^2 + x - 9", graph (\x -> x*x + x - 9) offRange)
         , ("x^3"        , graph (\x -> x*x*x) range)
         , ("Sin Wave"   , graph sin piRange)
         , ("Cosine Wave", graph cos piRange)
         , ("Scattered"  , graph (\x -> x + tan x) range)
         ]


----  Put it all on screen  ----

main = scene (dropDown styles) (dropDown points)

scene (styleDrop, style) (pointsDrop, points) =
  let f sty ps = flow down
         [ plot sty 400 400 ps
         , flow right [ plainText "Options: ", pointsDrop, styleDrop ]
         ]
  in lift2 f style points
