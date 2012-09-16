
module Website.Docs (createDocs) where

import Data.List (intersperse)

lightGrey  = rgb 245 245 245
mediumGrey = rgb 216 221 225

skeleton body outer =
  let content = body (outer - 80) in
  flow down [ spacer outer 15
            , container outer (heightOf content) midTop content
            , container outer 50 midBottom . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces px = intersperse (spacer 1 px)

section s = bold . Text.height s . toText

entry w (name, typ, desc) =
  let tipe = if length typ > 0 then " :: " ++ typ else "" in
  flow down
    [ color mediumGrey $ spacer w 1
    , width w . color lightGrey . text . monospace $ bold (toText name) ++ toText tipe
    , spacer 1 10
    , flow right [ spacer 50 10, width (w-50) $ plainText desc ]
    , spacer 1 20
    ]

group w (name, fs) =
  flow down $ text (section (5/4) name) : spacer 1 20 : map (entry w) fs

createDocs name cats =
  let f w = flow down $ [ text $ Text.link "/Documentation.elm" (toText "Back")
                        , width w . centeredText $ section 2 name
                        , spacer 1 30
                        ] ++ (addSpaces 50 $ map (group w) cats)
  in  lift (skeleton f) Window.width
