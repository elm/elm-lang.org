
module Website.Skeleton (skeleton, addSpaces) where

import Data.List (intersperse)

button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . bold . Text.height 2 . toText $ "Elm"

lightGrey = rgb (240/255) (241/255) (244/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , width outer . box 2 $ plainText "&nbsp;" `above` body inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces = map (\f -> f ()) . intersperse (\x -> plainText "&nbsp;") . map (\e x -> e)
