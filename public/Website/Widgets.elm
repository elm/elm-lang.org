module Website.Widgets (bigLogo, installButtons, button, headerFaces) where

import Graphics.Input as Input
import Text
import Website.ColorScheme as C

headerFaces =
    [ "futura", "century gothic", "twentieth century"
    , "calibri", "verdana", "helvetica", "arial"
    ]

bigLogo =
    let name = leftAligned << Text.height 60 <| toText "elm" in
    flow right [ image 80 80 "/logo.png"
               , spacer 10 80
               , container (widthOf name) 80 middle name
               ]

installButtons w =
  let href = "https://github.com/elm-lang/elm-platform/blob/master/README.md#elm-platform"
  in  flow right [ button (w // 2) 180 "/try" "Try"
                 , button (w // 2) 180 href "Install"
                 ]

-- implementation

click : Input.Input ()
click = Input.input ()

button : Int -> Int -> String -> String -> Element
button outerWidth innerWidth href msg =
    let box' = box innerWidth msg in
    container outerWidth 100 middle << link href <|
    Input.customButton click.handle ()
        (box' C.lightGrey C.mediumGrey)
        (box' C.lightGrey C.accent1)
        (box' C.mediumGrey C.accent1)

box : Int -> String -> Color -> Color -> Element
box w msg c1 c2 =
    let words = leftAligned << Text.height 26 << typeface faces << Text.color charcoal <| toText msg
    in
        container (w-2) 48 middle words
            |> color c1
            |> container w 50 middle
            |> color c2

faces : [String]
faces = [ "Lucida Grande"
        , "Trebuchet MS"
        , "Bitstream Vera Sans"
        , "Verdana"
        , "Helvetica"
        , "sans-serif"
        ]