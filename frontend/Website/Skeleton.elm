module Website.Skeleton where

import Color
import Graphics.Element (..)
import Graphics.Input as Input
import List
import Native.RedirectHack
import Signal
import Text
import Website.Widgets (headerFaces, logoImage)
import Website.ColorScheme as C


skeleton : String -> (Int -> Element) -> (Int,Int) -> Element
skeleton localName bodyFunc (w,h) =
  let body = bodyFunc w
      navBar = heading localName w
      bodyHeight = max (heightOf body) (h - heightOf navBar - 131)
  in color (Color.rgb 253 253 253) <| flow down
       [ navBar
       , container w bodyHeight midTop body
       , spacer w 80
       , color C.mediumGrey (spacer w 1)
       , color C.lightGrey <| container w 50 middle <| Text.centered footerWords
       ]

footerWords =
  let wordLink words1 href words2 words3 =
          Text.fromString words1 ++ Text.link href (Text.fromString words2) ++ Text.fromString words3
  in
     Text.color (Color.rgb 145 145 145) <|
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
          let name =
                Text.fromString "elm"
                  |> Text.height 24
                  |> Text.color clr
                  |> Text.leftAligned
          in
            color C.lightGrey <| 
            flow right
              [ logoImage 30 30
              , spacer 4 30
              , container (widthOf name) 30 middle name
              ]
    in
        link "/" <|
        Input.customButton
            (Signal.send clicks "/")
            (btn Color.charcoal)
            (btn Color.black)
            (btn Color.black)


tabs localName =
  flow right (List.map (tab localName) paths)


paths =
  [ ("Learn"    , "/Learn.elm")
  , ("Examples" , "/Examples.elm")
  , ("Packages" , "http://package.elm-lang.org/")
  , ("Community", "/Community.elm")
  , ("Blog"     , "/Blog.elm")
  , ("Install"  , "/Install.elm")
  ]

clicks : Signal.Channel String
clicks =
  Signal.channel ""


bad =
  Signal.map Native.RedirectHack.redirect (Signal.subscribe clicks)


tab localName (name, href) =
  let (accent, h) =
        if localName == name then (C.accent1, 3) else (C.mediumGrey, 1)

      btn clr =
          let words =
                Text.fromString name
                  |> Text.color clr
                  |> Text.leftAligned
          in
            flow down
              [ color C.lightGrey <| container (widthOf words + 20) 40 middle words
              , color accent (spacer (widthOf words + 20) h)
              ]
  in
    link href <|
      Input.customButton
          (Signal.send clicks href)
          (btn Color.charcoal)
          (btn Color.black)
          (btn Color.black)
