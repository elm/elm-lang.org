
module Website.Skeleton (skeleton) where

button (name, href) = link href . container 100 60 middle $ plainText name
buttons = flow right . map button $
  [ ("Home"    , "/"                 )
  , ("Examples", "/Examples.elm"     )
  , ("Docs"    , "/Documentation.elm")
  , ("Download", "/Download.elm"     ) ]

lightGrey  = rgb 245 245 245
mediumGrey = rgb 216 221 225

title w = container w 60 midLeft . text . Text.height 2 . bold $ toText "Elm"

heading outer inner =
  color mediumGrey . container outer 61 midTop .
  color  lightGrey . container outer 60 middle .
  container inner 60 middle $ title (inner - widthOf buttons) `beside` buttons

skeleton bodyFunc outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  let body = bodyFunc inner in
  flow down [ heading outer inner
            , spacer outer 10
            , container outer (heightOf body) middle body
            , container outer 50 midBottom . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]