-- Try this out on an iOS or Android device. For best results
-- use the "In Tab" compile option.

import Dict
import Touch
import Window

add1 : Touch -> Dict Int [(Int,Int)] -> Dict Int [(Int,Int)]
add1 t d = let vs = Dict.findWithDefault [] t.id d
           in  Dict.insert t.id ((t.x,t.y) :: vs) d

addN : [Touch] -> Dict Int [(Int,Int)] -> Dict Int [(Int,Int)]
addN ts dict = List.foldl add1 dict ts

thickLine : LineStyle
thickLine = { defaultLine | color <- rgba 123 123 123 0.3, width <- 8 }

scene : (Int,Int) -> [[(Int,Int)]] -> Element
scene (w,h) paths =
    let pathForms = group (map (traced thickLine . path) paths)
    in  collage w h [ move (0 - w `div` 2, h `div` 2) pathForms ]

main = lift2 scene Window.dimensions
                   (Dict.values <~ foldp addN Dict.empty Touch.touches)
