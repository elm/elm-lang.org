
import Signal.Mouse (isClickedOn)
import Signal.Window (dimensions)

--  Three Shades of Grey

c1 = rgba 255 255 255 (4/9)
c2 = rgba 110 110 110 (5/9)
c3 = rgb  244 244 244


--  Clickable left and right arrows

arrow theta = let { w = 30 ; h = 60 ; x = w * 2/7 ; y = h*3/11 } in
  collage (round w) (round h)
    [ filled c1 $ rect w h (w/2, h/2)
    , filled c2 . rotate theta $ polygon [ (x,0),(0-x,y),(0-x,0-y) ] (w/2,h/2)
    ]
(leftArrow , leftClicked ) = isClickedOn (arrow (1/2))
(rightArrow, rightClicked) = isClickedOn (arrow 0)


--  Helper functions

ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }
safeIth i lst = ith (i `mod` length lst) lst

countTrue = count . keepIf id True
index = lift2 (-) (countTrue rightClicked) (countTrue leftClicked)


--  Actual light-box code

lightBox boxX boxY imgs =
  let disp loc = size (boxX `div` 2) boxY . box loc in
  let scene (w,h) index =
    size w h . color c3 . box 5 $ layers
      [ disp 4 leftArrow `beside` disp 6 rightArrow
      , fittedImage boxX boxY (safeIth index imgs)
      ]
  in
     lift2 scene dimensions index

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]

main = lightBox 472 315 images
