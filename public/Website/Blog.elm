
module Website.Blog where

import JavaScript as JS
import open Website.ColorScheme
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

navigation = Input.customButtons ""

button (name, href, clr) =
 let btn alpha =
         flow down [ color (rgba 200 200 200 alpha) . container 100 24 middle .
                     width 100 . centered . Text.color black <| toText name
                   , color clr (spacer 100 2) ]
 in  link href <| navigation.customButton href (btn 0) (btn 0.1) (btn 0.2)

buttons = flow right . map button <|
  [ ("About"   , "/About.elm"        , accent1)
  , ("Examples", "/Examples.elm"     , accent2)
  , ("Docs"    , "/Documentation.elm", accent3)
  , ("Download", "/Download.elm"     , accent4) ]

title w =
 let logo = text . typeface "futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial" . Text.color lightGrey . Text.height 24 <| toText "elm"
 in  link "/" <| container w 30 middle logo

heading outer =
  topBar 10 outer

skeleton bodyFunc outer =
  let body = bodyFunc outer
  in color lightGrey <| flow down
       [ heading outer
       , spacer outer 10
       , container outer (heightOf body) middle body
       , container outer 50 midBottom . Text.centered <|
         Text.color (rgb 145 145 145) (Text.toText "&copy; 2011-2013 ") ++
             Text.link "https://github.com/evancz" (Text.toText "Evan Czaplicki")
       ]
