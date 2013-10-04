
module Website.Skeleton where

import open Website.ColorScheme
import Graphics.Input as Input

skeleton = flexSkeleton True 526
skeleton' = flexSkeleton True
homeSkeleton = flexSkeleton False 526

topBarHeight = 34
topBarPadding = 2
footerHeight = 40

extraHeight = topBarHeight + topBarPadding + footerHeight

flexSkeleton isNormal inner bodyFunc (w,h) =
    let content = bodyFunc (min inner w) in
    color lightGrey <|
    flow down [ topBar isNormal inner w
              , container w (max (h-extraHeight) (heightOf content)) midTop content
              , container w footerHeight (midBottomAt (relative 0.5) (absolute 10)) . Text.centered <|
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
                Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
              ]

topBar isNormal inner w =
    let leftWidth = inner - sum (map widthOf buttons)
        left = if isNormal then logo leftWidth else spacer leftWidth 30
    in  flow down
        [ container w topBarHeight middle . flow right <| left :: buttons
        , container w topBarPadding midTop <| color mediumGrey (spacer 800 1)
        ]

logo w =
    let name = text . Text.height 24 <| toText "elm" in
    container w topBarHeight midLeft . link "/" <|
    flow right [ image 30 30 "/logo.png"
               , spacer 4 30
               , container (widthOf name) 30 middle name
               ]

bigLogo =
    let name = text . Text.height 60 <| toText "elm" in
    flow right [ image 80 80 "/logo.png"
               , spacer 10 80
               , container (widthOf name) 80 middle name
               ]

buttons = map button paths

paths =
  [ ("Learn"    , "/Learn.elm")
  , ("Examples" , "/Examples.elm")
  , ("Docs"     , "http://docs.elm-lang.org")
  , ("Install"  , "/Install.elm")
  ]

button (name, href) =
    let words = text . Text.link href <| toText name
    in  container (widthOf words + 20) topBarHeight midRight words

