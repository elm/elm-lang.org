
module Website.SkeletonV2 where

import open Website.ColorScheme
import Graphics.Input as Input

skeleton bodyFunc w =
    color lightGrey <|
    flow down [ topBar w
              , bodyFunc w
              , container w 50 midBottom . Text.centered <|
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
                Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
              ]

topBar w =
    let logo = link "/" . container 70 30 middle <| image 30 30 "/logo.png"
    in  flow down
            [ container w 30 middle . flow right <|
              map button paths1 ++ logo :: map button paths2
            , container w 10 midTop <| color mediumGrey (spacer 800 1)
            ]

paths1 =
  [ ("Docs"     , "http://docs.elm-lang.org")
  , ("Learn"    , "/About.elm"   )
  , ("Examples" , "/Examples.elm")
  , ("Blog"     , "/Blog.elm")
  ]

paths2 = 
  [ ("Try"       , "/try")
  , ("Share"     , "http://www.share-elm.com/")
  , ("Install"   , "/Download.elm")
  , ("Contribute", "https://github.com/evancz/Elm")
  ]

button (name, href) =
    let words = text . Text.link href <| toText name
    in  container (widthOf words + 20) 30 middle words

