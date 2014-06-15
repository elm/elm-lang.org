
module Website.Blog where

import Website.ColorScheme as C
import Graphics.Input as Input
import Text

accents = [ C.accent0, C.accent1, C.accent2, C.accent3, C.accent4 ]

topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0::segs)
        addColors = zipWith color (accents ++ accents)
        box w = spacer w 5
        boxes = box :: title :: map (\_ -> box) [1..8]
    in  flow right <| addColors (zipWith (<|) boxes ws)

faces : [String]
faces = [ "futura", "century gothic", "twentieth century"
        , "calibri", "verdana", "helvetica", "arial"
        ]

logo : Element
logo =
    Text.leftAligned . Text.typeface faces . Text.color C.lightGrey . Text.height 30 <| Text.toText "elm"

title : Int -> Element
title w =
    link "/" <| container w 36 middle logo

heading outer =
  topBar 10 outer

skeleton : (Int -> Element) -> Int -> Element
skeleton bodyFunc outer =
  let body = bodyFunc outer
  in color C.lightGrey <| flow down
       [ heading outer
       , spacer outer 10
       , container outer (heightOf body) middle body
       , container outer 50 middle <| Text.centered footerWords
       ]

footerWords =
  let wordLink words1 href words2 words3 =
          Text.toText words1 ++ Text.link href (Text.toText words2) ++ Text.toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/elm-lang/elm-lang.org" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2011-14"
