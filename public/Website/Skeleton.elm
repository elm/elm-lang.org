
module Website.Skeleton where

import open Website.ColorScheme
import Graphics.Input as Input

skeleton = skeleton' 526

topBarHeight = 34
topBarPadding = 2
footerHeight = 40

extraHeight = topBarHeight + topBarPadding + footerHeight

skeleton' inner bodyFunc (w,h) =
    let content = bodyFunc (min inner w) in
    color lightGrey <|
    flow down [ topBar w
              , container w (max (h-extraHeight) (heightOf content)) midTop content
              , container w footerHeight (midBottomAt (relative 0.5) (absolute 10)) . Text.centered <|
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
                Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
              ]

topBar w =
    let logo = link "/" . container 70 topBarHeight middle <| image 30 30 "/logo.png"
    in  flow down
            [ container w topBarHeight middle . flow right <|
              map button paths1 ++ logo :: map button paths2
            , container w topBarPadding midTop <| color mediumGrey (spacer 800 1)
            ]

paths1 =
  [ ("Docs"     , "http://docs.elm-lang.org")
  , ("Learn"    , "/Learn.elm")
  , ("Examples" , "/Examples.elm")
  , ("Blog"     , "/Blog.elm")
  ]

paths2 = 
  [ ("Try"       , "/try")
  , ("Share"     , "http://www.share-elm.com/")
  , ("Install"   , "/Install.elm")
  , ("Contribute", "/Contribute.elm")
  ]

button (name, href) =
    let words = text . Text.link href <| toText name
    in  container (widthOf words + 20) topBarHeight middle words

