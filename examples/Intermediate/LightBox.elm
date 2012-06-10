
----  Shades of Grey  ----

c1 = rgba 1 1 1 (4/9)
c2 = rgba (3/7) (3/7) (3/7) (5/9)
c3 = rgb  (244/255) (244/255) (244/255)


----  Clickable left and right arrows  ----

arrow theta = let { w = 30 ; h = 60 ; x = w * 2/7 ; y = h*3/11 } in
  collage w h
    [ filled c1 $ rect w h (w/2, h/2)
    , filled c2 . rotate theta $ polygon [ (x,0),(0-x,y),(0-x,0-y) ] (w/2,h/2)
    ]
leftArrow  = Mouse.clickedOn $ arrow (1/2)
rightArrow = Mouse.clickedOn $ arrow 0


----  Helper functions  ----

fst (a,_) = a
snd (_,b) = b
countTrue = foldp (\b c -> if b then c + 1 else c) 0
listify = List.foldr (lift2 (:)) (constant [])
ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }
safeIth lst i = ith (i `mod` List.length lst) lst


----  Actual light-box code  ----

lightBox w h imgs =
 let { index = lift2 (-) (countTrue $ snd rightArrow) (countTrue $ snd leftArrow)
     ;  disp loc = constant . size (w/2) h . box loc . fst }
 in
 lift3 size Window.width Window.height . lift (color c3 . box 5 . layers) . listify $
  [ lift2 beside (disp 4 leftArrow) (disp 6 rightArrow)
  , lift (size w h . image . safeIth imgs) index
  ]

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]

main = lightBox 472 315 images
