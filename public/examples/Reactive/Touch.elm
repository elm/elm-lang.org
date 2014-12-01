
import Touch
import Window

main : Signal Element
main =
    lift2 scene Window.dimensions Touch.touches

type alias Touch = { x:Int, y:Int }

scene : (Int,Int) -> [Touch] -> Element
scene (w,h) touches =
    let dots = map (makeCircle (toFloat w) (toFloat h)) touches
    in 
        layers [ collage w h dots, message ]

makeCircle : Float -> Float -> Touch -> Form
makeCircle w h {x,y} =
    circle 60
        |> filled green
        |> move (toFloat x - w/2, h/2 - toFloat y)

message : Element
message = [markdown|

<a href="/examples/Reactive/Touch.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

|]
