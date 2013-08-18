
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
 let logo = text . typeface "futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial" . Text.color lightGrey . Text.height 20 <| toText "elm"
 in  link "/" <| container w 24 middle logo

heading outer =
  layers [ topBar 10 outer, style ]

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


style = [markdown|
<style type="text/css">
pre {
  margin: 0 30px;
  padding: 4px 10px;
  border-top:    solid 2px rgb(96,181,204);
  border-left:   solid 2px rgb(240,173,0);
  border-right:  solid 2px rgb(234,21,122);
  border-bottom: solid 2px rgb(127,209,59);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: white; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: white; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

|]