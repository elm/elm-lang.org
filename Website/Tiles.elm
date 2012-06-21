
module Website.Tiles (tile, toTile) where

import Data.List (take,drop)

format (x,y,z) = (x, z ++ y ++ ".elm", "/screenshot/" ++ y ++ ".jpg")

tileSize = 130

toTile info =
  let { (name, ex, pic) = format info
      ; x = tileSize }
  in  Element.link ("/edit/examples/" ++ ex) $ flow down
       [ size x x . box 5 . size (x-10) (x-10) $ image pic
       , width x . centeredText $ toText name
       ]

groups n lst =
  case lst of
  { [] -> [] ; x:xs -> take n lst : groups n (drop n lst) }

tile w tiles =
  flow down . addSpaces . map (flow right) $ groups (w/tileSize-1) tiles
