import Graphics.Input as Input

----  Create graphs from scratch  ----

data Style = Points | Line

plot style w h points =
  let (xs,ys) = unzip points
      eps = 26/25
      xmin = eps * minimum xs
      xmax = eps * maximum xs
      ymin = eps * minimum ys
      ymax = eps * maximum ys
      fit scale lo hi z = scale * abs (z-lo) / abs (hi-lo)
      f (x,y) = (fit w xmin xmax x, h - fit h ymin ymax y)
      axis a b = traced (solid black) . line . map f <| [a,b]
      xaxis = axis (xmin, clamp ymin ymax 0) (xmax, clamp ymin ymax 0)
      yaxis = axis (clamp xmin xmax 0, ymin) (clamp xmin xmax 0, ymax)
      draw ps = case style of
                  Points -> map (\p -> move p . outlined (solid blue) <| ngon 4 3) ps
                  Line   -> [ traced (solid blue) <| path ps ]
  in  collage (round w) (round h) <| [ xaxis, yaxis ] ++ draw (map f points)


-----  Provide many graphs for display  ----

range    = map toFloat [ -10 .. 10 ]
piRange  = map (\x -> toFloat x / 40 * pi) [-40..40]
offRange = map (\x -> toFloat x / 5) [-20..10]

graph f range = zip range (map f range)

polarGraph f thetas =
  let fromPolar theta r = (r * cos theta, r * sin theta) in
  zipWith fromPolar thetas (map f thetas)

styles = [ ("Line Graph", Line)
         , ("Scatter Plot", Points)
         ]

lissajous m n t = (cos (m*t), sin (n*t))

points = [ ("r = cos 4 theta" , polarGraph (\t -> cos (4*t)) piRange)
         , ("Lissajous"  , map (lissajous 3 2) piRange)
         , ("Circle"     , map (\t -> (cos t, sin t)) piRange)
         , ("x^2"        , graph (\x -> x*x) range)
         , ("x^2 + x - 9", graph (\x -> x*x + x - 9) offRange)
         , ("x^3"        , graph (\x -> x*x*x) range)
         , ("Sin Wave"   , graph sin piRange)
         , ("Cosine Wave", graph cos piRange)
         , ("Scattered"  , graph (\x -> x + tan x) range)
         ]


----  Put it all on screen  ----

main = scene (Input.dropDown styles) (Input.dropDown points)

scene (styleDrop, style) (pointsDrop, points) =
  let f sty ps = flow down
         [ plot sty 400 400 ps
         , flow right [ plainText "Options: ", pointsDrop, styleDrop ]
         ]
  in  lift2 f style points
