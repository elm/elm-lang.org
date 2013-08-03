
module Website.Skeleton where

import JavaScript as JS
import open Website.ColorScheme
import Graphics.Input as Input

navigation = Input.customButtons ""

button (name, href, clr) =
 let btn alpha =
         flow down [ color (rgba 200 200 200 alpha) . container 100 58 middle .
                     width 100 . centered . Text.color black <| toText name
                   , color clr (spacer 100 2) ]
 in  link href <| navigation.customButton href (btn 0) (btn 0.1) (btn 0.2)

buttons = flow right . map button <|
  [ ("About"   , "/About.elm"        , accent1)
  , ("Examples", "/Examples.elm"     , accent2)
  , ("Docs"    , "/Documentation.elm", accent3)
  , ("Download", "/Download.elm"     , accent4) ]

title w =
 let logo = text . Text.color black . Text.height 40 . bold <| toText "Elm"
 in  container w 60 midLeft (link "/" logo)

veiwSource = [markdown|
<a href="javascript:var p=top.location.pathname;if(p.slice(0,5)!='/edit')top.location.href='/edit'+(p=='/'?'/Elm.elm':p);">
<img style="position: absolute; top: 0; right: 0; border: 0;"
     src="/ribbon.gif"
     alt="View Page Source">
</a>
|]

heading outer inner =
  let header = container outer 60 middle <|
               title (inner - widthOf buttons) `beside` buttons
  in  layers <| [ flow down [ color lightGrey (spacer outer 58)
                            , color mediumGrey (spacer outer 1) ]
                , header ] ++
          (if outer < 800 then [] else [width outer veiwSource])

skeleton bodyFunc outer =
  let inner = if outer < 840 then outer - 40 else 800
      body = bodyFunc inner
  in flow down
       [ heading outer inner
       , spacer outer 10
       , container outer (heightOf body) middle body
       , container outer 50 midBottom . Text.centered <|
         Text.color (rgb 145 145 145) (Text.toText "&copy; 2011-2013 ") ++
             Text.link "https://github.com/evancz" (Text.toText "Evan Czaplicki")
       ]
