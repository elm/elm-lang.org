module Website.Skeleton where

import Website.Widgets (headerFaces)
import Website.ColorScheme as C
import Graphics.Input as Input
import Text

skeleton : String -> (Int -> Element) -> (Int,Int) -> Element
skeleton localName bodyFunc (w,h) =
  let body = bodyFunc w
      navBar = heading localName w
      bodyHeight = max (heightOf body) (h - heightOf navBar - 131)
  in color (rgb 253 253 253) <| flow down
       [ navBar
       , container w bodyHeight midTop body
       , spacer w 80
       , color C.mediumGrey (spacer w 1)
       , color C.lightGrey <| container w 50 middle <| Text.centered footerWords
       ]

footerWords =
  let wordLink words1 href words2 words3 =
          Text.toText words1 ++ Text.link href (Text.toText words2) ++ Text.toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/elm-lang/elm-lang.org" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2011-14"

heading localName outer =
  let inner = min 800 outer
      leftWidth = max 0 ((outer - inner) // 2)
      rightWidth = max 0 (outer - leftWidth - inner)
  in
  flow right
  [ color C.lightGrey (spacer leftWidth 40) `above` color C.mediumGrey (spacer leftWidth 1)
  , topBar localName inner
  , color C.lightGrey (spacer rightWidth 40) `above` color C.mediumGrey (spacer rightWidth 1)
  ]

topBar localName inner =
  let tabs' = tabs localName
      w = inner - widthOf tabs'
  in
  flow right
  [ flow down
    [ color C.lightGrey <| container w 40 midLeft logo
    , color C.mediumGrey (spacer w 1)
    ]
  , tabs'
  ]

logo =
    let btn clr =
            let name = leftAligned . Text.color clr . Text.height 24 <| toText "elm" in
            color C.lightGrey <| 
            flow right [ image 30 30 "/logo.png"
                       , spacer 4 30
                       , container (widthOf name) 30 middle name
                       ]
    in
        link "/" <|
        Input.customButton clicks.handle () (btn charcoal) (btn black) (btn black)

tabs localName = flow right (map (tab localName) paths)

paths =
  [ ("Learn"    , "/Learn.elm")
  , ("Examples" , "/Examples.elm")
  , ("Libraries", "/Libraries.elm")
  , ("Community", "/Community.elm")
  , ("Blog"     , "/Blog.elm")
  ]

clicks : Input.Input ()
clicks = Input.input ()

tab localName (name, href) =
    let (accent, h) = if localName == name then (C.accent1, 3) else (C.mediumGrey, 1)
        btn clr =
            let words = leftAligned . Text.color clr <| toText name
            in  flow down
                [ color C.lightGrey <| container (widthOf words + 20) 40 middle words
                , color accent (spacer (widthOf words + 20) h)
                ]
    in  link href <|
        Input.customButton clicks.handle () (btn charcoal) (btn black) (btn black)
