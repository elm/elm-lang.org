import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Text exposing (..)
import Window


main : Varying Element
main =
  Varying.map2 scene Window.dimensions clickLocations


-- for a good time, remove "sampleOn Mouse.clicks" ;)
clickLocations : Varying (List (Int,Int))
clickLocations =
  Stream.sample always Mouse.position Mouse.clicks
    |> Stream.fold (::) []


scene : (Int,Int) -> List (Int,Int) -> Element
scene (w,h) locs =
  let drawPentagon (x,y) =
          ngon 5 20
            |> filled (hsla (toFloat x) 0.9 0.6 0.7)
            |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
            |> rotate (toFloat x)
  in
      layers
        [ collage w h (List.map drawPentagon locs)
        , plainText "Click to stamp a pentagon."
        ]
