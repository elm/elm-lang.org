
module Website.Docs (createDocs) where

import open Website.ColorScheme
import Window
import Char

accents = [accent0,accent1,accent2,accent3,accent4]

topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0::segs)
        accentCycle = concatMap (\_ -> accents) [ 0 .. k `div` 5 ]
    in  flow right <| zipWith (\c w -> color c <| spacer w 5) accentCycle ws

skeleton body outer =
  let content = body (outer - 80) in
  flow down [ topBar 10 outer 
            , spacer outer 15
            , container outer (heightOf content) midTop content
            , container outer 50 midBottom . text <|
                Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
                    Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
            ]

addSpaces px = intersperse (spacer 1 px)

section s = bold . Text.height s . toText

entry f w (name, typ, desc) =
  let colons = Text.color accent1 <| toText " : "
      tipe = if Char.isUpper (head name)
             then bold (toText typ)
             else bold (toText name) ++ colons ++ toText typ
  in
  flow down
    [ color mediumGrey <| spacer w 1
    , tag name <| width w . color lightGrey . text . monospace <| tipe
    , flow right [ spacer 50 10, f (w-50) desc ]
    ]

f1 w c = flow down [ spacer 1 10, width w <| plainText c, spacer 1 20 ]
f2 w c = let c' = width w c
             pos = topLeftAt (absolute 0) (absolute (0-5))
         in  container w (heightOf c') pos c'

group f w (name, fs) =
  flow down <| text (section 20 name) :: spacer 1 20 :: map (entry f w) fs

createDocs name overview cats =
  let f w = flow down <| [ text <| Text.link "/Documentation.elm" (toText "Back")
                         , width w . centered <| section 40 name
                         , width w overview
                         , spacer 1 30
                         ] ++ (addSpaces 50 <| map (group f2 w) cats)
  in  lift (skeleton f) Window.width
