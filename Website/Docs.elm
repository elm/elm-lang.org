
module Website.Docs (createDocs) where

lightGrey = rgb (245/255) (245/255) (245/255)
mediumGrey = rgb (216/255) (221/255) (225/255)

skeleton body outer =
  flow down [ space 15 , width outer . box 2 . body $ outer - 80
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

space px = rectangle 1 px
addSpaces px = List.map (\f -> f ()) . List.intersperse (\x -> space px) . List.map (\e x -> e)

section s = bold . Text.height s . toText

entry w (name, type, desc) =
  let tipe = if List.length type > 0 then " :: " ++ type else "" in
  flow down
    [ color mediumGrey $ rectangle w 1
    , width w . color lightGrey . text . monospace $ bold (toText name) ++ toText tipe
    , space 10
    , flow right [ width 50 $ plainText "&nbsp;", width (w-50) . text . toText $ desc ]
    , space 20
    ]

group w (name, fs) =
  flow down $ text (section (5/4) name) : space 20 : List.map (entry w) fs

createDocs name cats =
  let f w = flow down $ [ text $ link "/Documentation.elm" (toText "Back")
                        , width w . centeredText $ section 2 name
                        , space 30 ] ++ (addSpaces 50 $ List.map (group w) cats) in
  lift (skeleton f) Window.width