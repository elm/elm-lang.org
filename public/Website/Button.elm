module Website.Button (button) where

import Graphics.Input as Input
import Text

click : Input.Input ()
click = Input.input ()

button : Int -> Int -> String -> String -> Element
button outerWidth innerWidth href msg =
    let box' = box innerWidth msg in
    container outerWidth 100 middle . link href <|
    Input.customButton click.handle ()
        (box' lightGrey grey)
        (box' lightGrey darkGrey)
        (box' grey blue)

box : Int -> String -> Color -> Color -> Element
box w msg c1 c2 =
    let words = leftAligned . Text.height 30 . typeface faces . Text.color charcoal <| toText msg
    in
        color c2 . container w 50 middle . color c1 <|
        container (w-2) 48 middle words

faces : [String]
faces = [ "Lucida Grande"
        , "Trebuchet MS"
        , "Bitstream Vera Sans"
        , "Verdana"
        , "Helvetica"
        , "sans-serif"
        ]