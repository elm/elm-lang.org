module Message (report) where

import Graphics.Element exposing (..)
import ColorScheme as C
import Markdown
import Window


scene msg (w,h) =
  container w h middle (box <| width 300 (Markdown.toElement msg))
    |> color C.mediumGrey


box e =
  let w = widthOf e
      h = heightOf e
  in
    flow down
      [ color C.accent1 (spacer (w+40) 5)
      , container (w+38) (h+10) midTop e
          |> color C.lightGrey
          |> container (w+40) (h+11) midTop
          |> color C.mediumGrey
      ]


report msg =
    Signal.map (scene msg) Window.dimensions