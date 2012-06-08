
ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]
slideShow (w,h) index =
  let i = index `mod` List.length images in
  size w h . color black . box 5 . image $ ith i images


clickCount = foldp (\b c -> if b then c + 1 else c) 0 Mouse.isClicked
tickCount t = foldp (\x c -> c + 1) 0 (Time.every t)


-- Be sure to also try this with `clickCount` which switches images
-- each time the user clicks the mouse. You can also try changing the
-- time interval (i.e. `tickCount 7`)

main = lift2 slideShow Window.dimensions (tickCount 4)