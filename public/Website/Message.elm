
module Website.Message (report) where

import Website.ColorScheme as C
import Window

accents = [ C.accent0, C.accent1, C.accent2, C.accent3, C.accent4 ]

topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0::segs)
        accentCycle = concatMap (\_ -> accents) [ 0 .. k `div` 5 ]
    in  flow right <| zipWith (\c w -> color c <| spacer w 5) accentCycle ws

scene msg (w,h) =
    color C.mediumGrey <| container w h middle (box <| width 300 msg)

box e =
  let w = widthOf e
      h = heightOf e
  in  flow down [ topBar 5 (w+40)
                , color C.mediumGrey . container (w+40) (h+11) midTop .
                  color C.lightGrey <| container (w+38) (h+10) midTop e
                ]

report msg = scene msg <~ Window.dimensions