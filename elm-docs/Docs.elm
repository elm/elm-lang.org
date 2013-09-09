module Docs where

import Window

accent0 = rgb 90 99 120

accent1 = rgb 96 181 204
accent2 = rgb 240 173 0
accent3 = rgb 234 21 122
accent4 = rgb 127 209 59

lightGrey  = rgb 245 245 245
mediumGrey = rgb 216 221 225
darkGrey   = rgb 164 166 169

documentation name es (w,h) search results =
    let title wid = container wid 100 middle . text . Text.height 60 <| toText name
        stuff = flow down (map (\e -> e (w-300)) (title :: es))
        maxH = max h (heightOf stuff)
    in  
      flow right [ sideBar maxH search results
                 , color mediumGrey <| spacer 1 maxH
                 , spacer 19 maxH
                 , stuff
                 , spacer 20 maxH ]

border e = color mediumGrey <| container (widthOf e + 2) (heightOf e + 2) middle e
linkName w str =
    let address = "/" ++ map (\c -> if c == '.' then '/' else c) str ++ ".elm"
    in  link address . width w . text . Text.color black <| toText str
sideBar h search results =
    color lightGrey . height h <|
    flow down [ link "/" <| fittedImage 260 100 "/tangram-logo.png"
              , container 260 50 middle . border <| width 200 search
              , spacer 260 20
              , flow right [ spacer 40 10
                           , flow down (intersperse (spacer 180 4) <|
                                 map (linkName 180) results)
                           ]
              ]

entry : String -> String -> Maybe (String,Int) -> Element -> Int -> Element
entry name tipe assocPrec prose w =
    let box n pos txt = container w (heightOf txt + n) pos txt
        ap = case assocPrec of
               Nothing -> []
               Just (a,p) -> [ box 0 midRight . text . Text.color mediumGrey . monospace . toText <|
                                   a ++ "-associative, precedence " ++ show p ]
    in
      flow down [ color lightGrey (spacer w 1) 
                , spacer w 8
                , layers <| ap ++ [ box 0 midLeft . text <| prettify tipe ]
                , width w prose
                , spacer w 16
                ]

until c xs =
  case xs of
    [] -> ([],[])
    hd::tl ->
        if | hd == '(' -> let (chomped,rest) = until ')' tl
                              (before,after) = until c rest
                          in  (hd :: chomped ++ before, after)
           | hd == c -> ([], xs)
           | otherwise -> let (before,after) = until c tl
                          in  (hd::before, after)

prettify raw =
    let firstFive = take 5 raw in
    if | firstFive == "type " || firstFive == "data " ->
           let (name, rest) = until ' ' (drop 5 raw) in
           monospace <| concat [ Text.color accent1 <| toText firstFive
                               , bold <| toText name
                               , colorize [] rest
                               ]
       | otherwise ->
           let (name, rest) = until ':' raw
           in  monospace <| bold (toText name) ++ colorize [] rest

colorize stuff str =
  let continue clr op rest =
          toText (reverse stuff) ++ Text.color clr (toText op) ++ colorize [] rest
  in 
  case str of
    ':' :: rest -> continue accent1 ":" rest
    '-' :: '>' :: rest -> continue accent1 "->" rest
    '|' :: rest -> continue accent1 "|" rest
    '=' :: rest -> continue accent1 "=" rest
    c :: rest -> colorize (c::stuff) rest
    [] -> toText (reverse stuff)
