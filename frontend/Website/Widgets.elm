module Website.Widgets (bigLogo, logoImage, installButtons, button, headerFaces) where

import Color
import Graphics.Element exposing (..)
import Graphics.Input as Input
import Text
import Website.ColorScheme as C


headerFaces =
    [ "futura", "century gothic", "twentieth century"
    , "calibri", "verdana", "helvetica", "arial"
    ]

logoImage w h =
  let smaller =
        toFloat
          >> (*) 0.8
          >> round
  in
    container w h middle <| image (smaller w) (smaller h) "/logo.svg"

bigLogo =
  let name =
        Text.fromString "elm"
          |> Text.height 60
          |> leftAligned
  in
    flow right
      [ logoImage 80 80
      , spacer 10 80
      , container (widthOf name) 80 middle name
      ]


installButtons w =
    flow right
    [ button (w // 2) 180 "/try" "Try"
    , button (w // 2) 180 "/Install.elm" "Install"
    ]


-- implementation

button : Int -> Int -> String -> String -> Element
button outerWidth innerWidth href msg =
    container outerWidth 100 middle << link href <|
        box innerWidth msg C.lightGrey C.mediumGrey


box : Int -> String -> Color.Color -> Color.Color -> Element
box w msg c1 c2 =
    let words =
          Text.fromString msg
            |> Text.color Color.charcoal
            |> Text.typeface faces
            |> Text.height 26
            |> leftAligned
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
