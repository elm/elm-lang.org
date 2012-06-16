
module Website.Tiles (tile, toTile) where

import Data.List

format (x,y,z) = (x, z ++ y ++ ".elm", "/screenshot/" ++ y ++ ".jpg")

tileSize = 130

toTile info =
  let { (name, ex, pic) = format info
      ; x = tileSize }
  in  Element.link ("/edit/examples/" ++ ex) $ flow down
       [ size x x . box 5 . size (x-10) (x-10) $ image pic
       , width x . centeredText $ toText name
       ]

take n lst = if n <= 0 then [] else
             case lst of { x:xs -> x : take (n-1) xs ; [] -> [] }
drop n lst = if n <= 0 then lst else
             case lst of { x:xs -> drop (n-1) xs ; [] -> [] }
groups n lst =
  case lst of
  { [] -> [] ; x:xs -> take n lst : groups n (drop n lst) }

tile w tiles =
  flow down . addSpaces . map (flow right) $ groups (w/tileSize-1) tiles
