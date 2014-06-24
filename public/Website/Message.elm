
module Website.Message (report) where

import Website.ColorScheme as C
import Window

scene msg (w,h) =
    color C.mediumGrey <| container w h middle (box <| width 300 msg)

box e =
  let w = widthOf e
      h = heightOf e
  in  flow down [ color C.accent1 (spacer (w+40) 5)
                , color C.mediumGrey . container (w+40) (h+11) midTop .
                  color C.lightGrey <| container (w+38) (h+10) midTop e
                ]

report msg = scene msg <~ Window.dimensions