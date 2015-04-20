import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input as Input
import List exposing (map, map2, unzip)


----  Put it all on screen  ----

style : Signal.Mailbox Style
style =
  Signal.mailbox Line


points : Signal.Mailbox (List (Float,Float))
points =
  Signal.mailbox lissajous


main : Signal Element
main =
  Signal.map2 view style.signal points.signal


view : Style -> List (Float,Float) -> Element
view currentStyle currentPoints =
  flow down
    [ plot currentStyle 400 400 currentPoints
    , flow right
        [ show "Options: "
        , Input.dropDown (Signal.message points.address) pointOptions
        , Input.dropDown (Signal.message style.address) styleOptions
        ]
    ]


----  Graph Styles  ----

type Style = Points | Line


styleOptions : List (String, Style)
styleOptions =
  [ ("Line Graph", Line)
  , ("Scatter Plot", Points)
  ]


----  Many graphs for display  ----

lissajous : List (Float,Float)
lissajous =
  let point m n t =
        (cos (m*t), sin (n*t))
  in
      map (point 3 2) piRange


pointOptions : List (String, List (Float,Float))
pointOptions =
  [ ("r = cos(4t)", polarGraph (\t -> cos (4*t)) piRange)
  , ("Lissajous"  , lissajous)
  , ("Circle"     , map (\t -> (cos t, sin t)) piRange)
  , ("x^2"        , graph (\x -> x*x) range)
  , ("x^2 + x - 9", graph (\x -> x*x + x - 9) offRange)
  , ("x^3"        , graph (\x -> x*x*x) range)
  , ("Sin Wave"   , graph sin piRange)
  , ("Cosine Wave", graph cos piRange)
  , ("Scattered"  , graph (\x -> x + tan x) range)
  ]


range : List Float
range =
  map toFloat [ -10 .. 10 ]


piRange : List Float
piRange =
  map (\x -> toFloat x / 40 * pi) [-40..40]


offRange : List Float
offRange =
  map (\x -> toFloat x / 5) [-20..10]


graph : (Float -> Float) -> List Float -> List (Float,Float)
graph f range =
  map2 (,) range (map f range)


polarGraph : (Float -> Float) -> List Float -> List (Float,Float)
polarGraph f thetas =
  map2 (\r t -> fromPolar (r,t)) (map f thetas) thetas


----  Render graphs from scratch  ----

plot : Style -> Float -> Float -> List (Float,Float) -> Element
plot style w h points =
  let (xs,ys) = unzip points
      eps = 26/25
      (xmin, xmax) = (eps * minimum xs, eps * maximum xs)
      (ymin, ymax) = (eps * minimum ys, eps * maximum ys)
      fit scale lo hi z = scale * abs (z-lo) / abs (hi-lo)
      f (x,y) = (fit w xmin xmax x, fit h ymin ymax y)
      axis a b = traced (solid black) <| path (map f [a,b])
      xaxis = axis (xmin, clamp ymin ymax 0) (xmax, clamp ymin ymax 0)
      yaxis = axis (clamp xmin xmax 0, ymin) (clamp xmin xmax 0, ymax)
      draw ps =
        case style of
          Points -> map (\p -> move p (outlined (solid lightBlue) (ngon 4 3))) ps
          Line   -> [ traced (solid lightBlue) (path ps) ]
  in
      collage (round w) (round h)
      [ [ xaxis, yaxis ] ++ draw (map f points)
          |> group
          |> move (-200,-200)
      ]


minimum : List Float -> Float
minimum numbers =
  Maybe.withDefault 0 (List.minimum numbers)


maximum : List Float -> Float
maximum numbers =
  Maybe.withDefault 0 (List.maximum numbers)