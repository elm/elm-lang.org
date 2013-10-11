import Graphics.Input as Input

-----  Provide many graphs for display  ----

lissajous m n t = (cos (m*t), sin (n*t))

points = [ ("r = cos(4t)", polarGraph (\t -> cos (4*t)) piRange)
         , ("Lissajous"  , map (lissajous 3 2) piRange)
         , ("Circle"     , map (\t -> (cos t, sin t)) piRange)
         , ("x^2"        , graph (\x -> x*x) range)
         , ("x^2 + x - 9", graph (\x -> x*x + x - 9) offRange)
         , ("x^3"        , graph (\x -> x*x*x) range)
         , ("Sin Wave"   , graph sin piRange)
         , ("Cosine Wave", graph cos piRange)
         , ("Scattered"  , graph (\x -> x + tan x) range)
         ]

range    = map toFloat [ -10 .. 10 ]
piRange  = map (\x -> toFloat x / 40 * pi) [-40..40]
offRange = map (\x -> toFloat x / 5) [-20..10]

graph f range = zip range (map f range)

polarGraph f thetas =
    zipWith (\r t -> fromPolar (r,t)) (map f thetas) thetas

styles = [ ("Line Graph", Line), ("Scatter Plot", Points) ]


----  Render graphs from scratch  ----

data Style = Points | Line

plot style w h points =
  let (xs,ys) = unzip points
      eps = 26/25
      (xmin, xmax) = (eps * minimum xs, eps * maximum xs)
      (ymin, ymax) = (eps * minimum ys, eps * maximum ys)
      fit scale lo hi z = scale * abs (z-lo) / abs (hi-lo)
      f (x,y) = (fit w xmin xmax x, fit h ymin ymax y)
      axis a b = traced (solid black) . path . map f <| [a,b]
      xaxis = axis (xmin, clamp ymin ymax 0) (xmax, clamp ymin ymax 0)
      yaxis = axis (clamp xmin xmax 0, ymin) (clamp xmin xmax 0, ymax)
      draw ps = case style of
                  Points -> map (\p -> move p . outlined (solid blue) <| ngon 4 3) ps
                  Line   -> [ traced (solid blue) <| path ps ]
  in  collage (round w) (round h) [ move (-200,-200) . group <| [ xaxis, yaxis ] ++ draw (map f points) ]


----  Put it all on screen  ----

(styleDrop, style) = Input.dropDown styles
(pointDrop, point) = Input.dropDown points

main = lift4 scene styleDrop style pointDrop point

scene styleDrop style pointsDrop points =
  flow down
    [ plot style 400 400 points
    , flow right [ plainText "Options: ", pointsDrop, styleDrop ]
    ]