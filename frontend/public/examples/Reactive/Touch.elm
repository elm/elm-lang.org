import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Markdown
import Touch
import Window


main : Varying Element
main =
    Varying.map2 scene Window.dimensions Touch.touches


scene : (Int,Int) -> List Touch.Touch -> Element
scene (w,h) touches =
    let dots = List.map (makeCircle (toFloat w) (toFloat h)) touches
    in
        layers [ collage w h dots, message ]


makeCircle : Float -> Float -> Touch.Touch -> Form
makeCircle w h {x,y} =
    circle 60
        |> filled green
        |> move (toFloat x - w/2, h/2 - toFloat y)


message : Element
message = Markdown.toElement """

<a href="/examples/Reactive/Touch.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

"""
