module Website.Skeleton (skeleton, skeleton', homeSkeleton, installButtons, bigLogo) where

import Website.Button as B
import Website.ColorScheme as C

skeleton : (Int -> Element) -> (Int,Int) -> Element
skeleton = flexSkeleton True 526

skeleton' : Int -> (Int -> Element) -> (Int,Int) -> Element
skeleton' = flexSkeleton True

homeSkeleton : (Int -> Element) -> (Int,Int) -> Element
homeSkeleton = flexSkeleton False 526

topBarHeight = 42
topBarPadding = 2
footerHeight = 120

extraHeight = topBarHeight + topBarPadding + footerHeight

flexSkeleton : Bool -> Int -> (Int -> Element) -> (Int,Int) -> Element
flexSkeleton isNormal inner bodyFunc (w,h) =
    let content = bodyFunc (min inner w) in
    color C.lightGrey <|
    flow down [ topBar isNormal inner w
              , container w (max (h-extraHeight) (heightOf content)) midTop content
              , container w footerHeight midBottom . flow down <|
                [ container w 2 middle . color C.mediumGrey <| spacer (inner+80) 1
                , container w 50 middle <| centered footerWords
                ]
              ]

topBar isNormal inner w =
    let leftWidth = inner - sum (map widthOf tabs)
        left = if isNormal then logo leftWidth else spacer leftWidth 30
    in  flow down
        [ container w topBarHeight middle . flow right <| left :: tabs
        , container w topBarPadding midTop <| color C.mediumGrey (spacer (inner+80) 1)
        ]

logo w =
    let name = leftAligned . Text.height 24 <| toText "elm" in
    container w topBarHeight midLeft . link "/" <|
    flow right [ image 30 30 "/logo.png"
               , spacer 4 30
               , container (widthOf name) 30 middle name
               ]

bigLogo =
    let name = leftAligned . Text.height 60 <| toText "elm" in
    flow right [ image 80 80 "/logo.png"
               , spacer 10 80
               , container (widthOf name) 80 middle name
               ]

tabs = map tab paths

paths =
  [ ("Learn"    , "/Learn.elm")
  , ("Examples" , "/Examples.elm")
  , ("Libraries", "/Libraries.elm")
  , ("Install"  , "/Install.elm")
  ]

tab (name, href) =
    let words = leftAligned . Text.link href <| toText name
    in  container (widthOf words + 20) topBarHeight midRight words

footerWords =
  let wordLink words1 href words2 words3 =
          toText words1 ++ Text.link href (toText words2) ++ toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/elm-lang/elm-lang.org" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2011-14"

installButtons w =
  let href = "https://github.com/elm-lang/elm-platform/blob/master/README.md#elm-platform"
  in  flow right [ B.button (w `div` 2) 180 "/try" "Try"
                 , B.button (w `div` 2) 180 href "Install"
                 ]
