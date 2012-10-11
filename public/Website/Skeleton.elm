
module Website.Skeleton (skeleton) where

a0 = rgb 90 99 120

a1 = rgb 96 181 204
a2 = rgb 240 173 0
a3 = rgb 234 21 122
a4 = rgb 127 209 59 --212 212 214

button (name, href, clr) =
    let accent = color clr (spacer 100 2) in
    let butn = container 100 58 middle $ text . Text.color black $ toText name in
    link href $ accent `below` butn

buttons = flow right . map button $
  [ ("Home"    , "/"                 , a1)
  , ("Examples", "/Examples.elm"     , a2)
  , ("Docs"    , "/Documentation.elm", a3)
  , ("Download", "/Download.elm"     , a4) ]

lightGrey  = rgb 245 245 245
mediumGrey = rgb 216 221 225

title w = container w 60 midLeft . text . Text.height 2 . bold $ toText "Elm"

heading outer inner =
  let header = container outer 60 middle $
               title (inner - widthOf buttons) `beside` buttons
  in  layers [ flow down [ color lightGrey (spacer outer 58)
                         , color mediumGrey (spacer outer 1) ]
             , header ]

skeleton bodyFunc outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  let body = bodyFunc inner in
  flow down [ heading outer inner
            , spacer outer 10
            , container outer (heightOf body) middle body
            , container outer 50 midBottom . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]