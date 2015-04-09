import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


main : Signal Element 
main =
  Signal.map colorCycle (Signal.foldp (+) 0 (fps 30))


colorCycle : Float -> Element
colorCycle time =
  let toDot angle =
        circle (20 + 10 * sin (angle * 2 + inSeconds time))
          |> filled (hsl angle 0.9 0.6)
          |> move (fromPolar (100, angle + pi))

      dots =
        List.map (\n -> toDot (turns (n/12))) [0..11]
  in
      collage 300 300 dots
