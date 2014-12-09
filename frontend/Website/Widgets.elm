module Website.Widgets (bigLogo, installButtons, button, headerFaces) where

import Color
import Graphics.Element (..)
import Graphics.Input as Input
import Native.RedirectHack
import Signal
import Text
import Website.ColorScheme as C


headerFaces =
    [ "futura", "century gothic", "twentieth century"
    , "calibri", "verdana", "helvetica", "arial"
    ]


bigLogo =
  let name =
        Text.fromString "elm"
          |> Text.height 60
          |> Text.leftAligned
  in
    flow right
      [ image 80 80 "/logo.png"
      , spacer 10 80
      , container (widthOf name) 80 middle name
      ]


installButtons w =
    flow right
    [ button (w // 2) 180 "/try" "Try"
    , button (w // 2) 180 "/Install.elm" "Install"
    ]


-- implementation

clicks : Signal.Channel String
clicks =
  Signal.channel ""


bad =
  Signal.map Native.RedirectHack.redirect (Signal.subscribe clicks)


button : Int -> Int -> String -> String -> Element
button outerWidth innerWidth href msg =
    let box' = box innerWidth msg in
    container outerWidth 100 middle << link href <|
    Input.customButton (Signal.send clicks href)
        (box' C.lightGrey C.mediumGrey)
        (box' C.lightGrey C.accent1)
        (box' C.mediumGrey C.accent1)


box : Int -> String -> Color.Color -> Color.Color -> Element
box w msg c1 c2 =
    let words =
          Text.fromString msg
            |> Text.color Color.charcoal
            |> Text.typeface faces
            |> Text.height 26
            |> Text.leftAligned
    in
        container (w-2) 48 middle words
            |> color c1
            |> container w 50 middle
            |> color c2


faces : List String
faces =
  [ "Lucida Grande"
  , "Trebuchet MS"
  , "Bitstream Vera Sans"
  , "Verdana"
  , "Helvetica"
  , "sans-serif"
  ]