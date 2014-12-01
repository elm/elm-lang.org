
import Dict
import Touch
import Window

main : Signal Element
main =
    lift2 scene
        Window.dimensions
        (Dict.values <~ foldp addN Dict.empty Touch.touches)

addN : [Touch.Touch] -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
addN ts dict =
    foldl add1 dict ts

add1 : Touch.Touch -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
add1 t d =
    let vs = Dict.getOrElse [] t.id d
    in
        Dict.insert t.id ((t.x,t.y) :: vs) d

scene : (Int,Int) -> [[(Int,Int)]] -> Element
scene (w,h) paths =
    let float (a,b) = (toFloat a, toFloat -b)
        pathForms = group (map (traced thickLine << path << map float) paths)
        picture = collage w h [ move (float (-w // 2, -h // 2)) pathForms ]
    in
        layers [ picture, message ]

thickLine : LineStyle
thickLine =
    { defaultLine |
        color <- rgba 123 123 123 0.3,
        width <- 8
    }

message = [markdown|

<a href="/examples/Reactive/Draw.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

|]

