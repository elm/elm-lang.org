-- Try this out on an iOS or Android device.

import Dict
import Touch
import Window

add1 : Touch.Touch -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
add1 t d = let vs = Dict.findWithDefault [] t.id d
           in  Dict.insert t.id ((t.x,t.y) :: vs) d

addN : [Touch.Touch] -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
addN ts dict = foldl add1 dict ts

cartesianTouch : (Int, Int) -> [Touch.Touch] -> [Touch.Touch]
cartesianTouch (w, h) ts = let transformTouch t = { t | x <- t.x - (div w 2), y <- -t.y + (div h 2) }
                           in map transformTouch ts

thickLine : LineStyle
thickLine = { defaultLine | color <- rgba 123 123 123 0.3, width <- 8 }

scene : (Int,Int) -> [[(Int,Int)]] -> Element
scene (w,h) paths =
    let float (a,b) = (toFloat a, toFloat b)
        pathForms = group (map (traced thickLine . path . map float) paths)
    in  collage w h [ pathForms ]

cartesianTouches : Signal [Touch.Touch]
cartesianTouches = cartesianTouch <~ Window.dimensions ~ Touch.touches

main = lift2 scene Window.dimensions
                   (Dict.values <~ foldp addN Dict.empty cartesianTouches)
