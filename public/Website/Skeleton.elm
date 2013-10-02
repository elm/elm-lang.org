
module Website.Skeleton where

import open Website.ColorScheme
import Graphics.Input as Input

skeleton = skeleton' 526

skeleton' inner bodyFunc (w,h) =
    let content = bodyFunc (min inner w) in
    color lightGrey <|
    flow down [ topBar w
              , container w (max (h-80) (heightOf content)) midTop content
              , container w 50 midBottom . Text.centered <|
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
                Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
              ]

topBar w =
    let logo = link "/" . container 70 32 middle <| image 30 30 "/logo.png"
    in  flow down
            [ container w 32 middle . flow right <|
              map button paths1 ++ logo :: map button paths2
            , container w 10 midTop <| color mediumGrey (spacer 800 1)
            ]

paths1 =
  [ ("Docs"     , "http://docs.elm-lang.org")
  , ("Learn"    , "/About.elm")
  , ("Examples" , "/Examples.elm")
  , ("Blog"     , "/Blog.elm")
  ]

paths2 = 
  [ ("Try"       , "/try")
  , ("Share"     , "http://www.share-elm.com/")
  , ("Install"   , "/Download.elm")
  , ("Contribute", "/Contribute.elm")
  ]

button (name, href) =
    let words = text . Text.link href <| toText name
    in  container (widthOf words + 20) 32 middle words

