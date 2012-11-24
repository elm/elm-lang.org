
module Website.Skeleton where

import Website.ColorScheme

button (name, href, clr) =
    let accent = color clr (spacer 100 2) in
    let butn = container 100 58 middle $ text . Text.color black $ toText name in
    Graphics.link href $ accent `below` butn

buttons = flow right . map button $
  [ ("About"   , "/About.elm"        , accent1)
  , ("Examples", "/Examples.elm"     , accent2)
  , ("Docs"    , "/Documentation.elm", accent3)
  , ("Download", "/Download.elm"     , accent4) ]

title w =
  let elm = text . Text.link "/" . Text.color black . Text.height 2 . bold $ toText "Elm" in
  container w 60 midLeft elm

veiwSource = [markdown|
<a href="javascript:var p=top.location.pathname;if(p.slice(0,5)!='/edit')top.location.href='/edit'+(p=='/'?'/Elm.elm':p);">
<img style="position: absolute; top: 0; right: 0; border: 0;"
     src="/ribbon.gif"
     alt="View Page Source">
</a>
|]

heading outer inner =
  let header = container outer 60 middle $
               title (inner - widthOf buttons) `beside` buttons
  in  layers [ flow down [ color lightGrey (spacer outer 58)
                         , color mediumGrey (spacer outer 1) ]
             , header
             , width outer veiwSource ]

skeleton bodyFunc outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  let body = bodyFunc inner in
  flow down [ heading outer inner
            , spacer outer 10
            , container outer (heightOf body) middle body
            , container outer 50 midBottom . text $
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2012 ") ++
                    Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
            ]