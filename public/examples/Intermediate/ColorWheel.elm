
import Website.ColorScheme

button str =
  let style = text . Text.height 2 . bold . Text.color mediumGrey . toText
  in  color lightGrey $ container 50 50 middle (style str)

(lButton, lclkd) = Mouse.isClickedOn (button "&larr;")
(rButton, rclkd) = Mouse.isClickedOn (button "&rarr;")

target = lift2 (-) (countIf id rclkd) (countIf id lclkd)


step inp (target,angle) =
    case inp of
      Left t -> (t, angle)
      Right dt -> (target, angle + dt * (target - angle) / 100)

input = mergeEither target (40 `fpsWhen` (second `since` target))


follower angle =
  let toRad t = (angle + t - 90) * pi / 180
      toPos t = (150 + 100 * cos (toRad t), 150 + 100 * sin (toRad t))
      toClr t = hsv (t `mod` 360) 1 1
      toRds t = let a = abs (abs ((t + angle) `mod` 360 - 180) - 180) in
                if a < 30 then 10 + (30-a) / 3 else 10
      toDot t = filled (toClr t) $ circle (toRds t) (toPos t)
  in  layers [ collage 300 300 $ map (toDot . ((*) 30)) [0..11]
             , container 300 300 middle . text . Text.height 2 . toText $
               show (round angle `mod` 360) ++ "&deg;" ]


scene (w,h) angle =
  container w h middle $
    layers [ follower (30 * angle)
           , flow right [ container 150 300 topLeft lButton
                        , container 150 300 topRight rButton ]
           ]

main = lift2 scene Window.dimensions (lift snd (foldp step (0,0) input))

