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

scene : (Int,Int) -> [(Int,Int)] -> Element
scene (w,h) = collage w h . List.map (traced (solid green) . path)

main = lift2 scene Window.dimensions
                   (Dict.values <~ foldp addN Dict.empty Touch.touches)
