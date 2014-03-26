
module Website.Blog where

import Website.ColorScheme (..)
import Graphics.Input as Input

accents = [accent0,accent1,accent2,accent3,accent4]

topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0::segs)
        addColors = zipWith color (accents ++ accents)
        box w = spacer w 5
        boxes = box :: title :: map (\_ -> box) [1..8]
    in  flow right <| addColors (zipWith (<|) boxes ws)

click = Input.input ""

button (name, href, clr) =
 let btn alpha =
         flow down [ color (rgba 200 200 200 alpha) . container 100 24 middle .
                     width 100 . leftAligned . Text.color black <| toText name
                   , color clr (spacer 100 2) ]
 in  link href <| Input.customButton click.handle href (btn 0) (btn 0.1) (btn 0.2)

buttons = flow right . map button <|
  [ ("About"   , "/About.elm"        , accent1)
  , ("Examples", "/Examples.elm"     , accent2)
  , ("Docs"    , "/Documentation.elm", accent3)
  , ("Download", "/Download.elm"     , accent4) ]

faces : [String]
faces = [ "futura", "century gothic", "twentieth century"
        , "calibri", "verdana", "helvetica", "arial"
        ]

logo : Element
logo =
    leftAligned . typeface faces . Text.color lightGrey . Text.height 30 <| toText "elm"

title : Int -> Element
title w =
    link "/" <| container w 36 middle logo

heading outer =
  topBar 10 outer

skeleton bodyFunc outer =
  let body = bodyFunc outer
  in color lightGrey <| flow down
       [ heading outer
       , spacer outer 10
       , container outer (heightOf body) middle body
       , container outer 50 middle <| centered footerWords
       ]

footerWords =
  let wordLink words1 href words2 words3 =
          toText words1 ++ Text.link href (toText words2) ++ toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/evancz/elm-lang.org" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2011-14"
