import Mouse
import Window

main : Signal Element
main = lift2 scene Mouse.position Window.dimensions

scene : (Int,Int) -> (Int,Int) -> Element
scene (x,y) (w,h) = pieChart <| map toFloat [x,y,w,h]

pieChart : [Float] -> Element
pieChart numbers =
    let fracs = normalize numbers
        offsets = scanl (+) 0 fracs
    in  collage 300 300 <|
        concat (zipWith3 (pieSlice 100) colors offsets fracs) ++ [ filled white (circle 70) ]

pieSlice : Float -> Color -> Float -> Float -> [Form]
pieSlice radius colr offset angle =
    let makePoint t = fromPolar (radius, degrees (360 * offset + t))
    in  [ filled colr . polygon <| (0,0) :: map makePoint[ 0 .. 360 * angle ]
        , toForm (asPercent angle)
            |> move (fromPolar (radius*1.25, turns (offset + angle/2)))
        ]

asPercent : Float -> Element
asPercent fraction =
    plainText <| show (toFloat . truncate <| fraction * 100) ++ "%"

colors : [Color]
colors = [ lightBlue, lightGreen, lightYellow, lightRed
         , lightPurple, blue, green, yellow, red, purple ]

normalize : [Float] -> [Float]
normalize xs =
    let total = sum xs
    in  map (\x -> x/total) xs
