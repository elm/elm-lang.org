
-- Creating the slide-show

ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]
slideShow (w,h) index =
  let i = index `mod` List.length images in
  size w h . color black . box 5 . image $ ith i images


-- Two ways to run the slide-show 

clickCount = foldp (\b c -> if b then c + 1 else c) 0 Mouse.isClicked
tickCount t = foldp (\x c -> c + 1) 0 (Time.every t)


-- Putting it all together

main = lift2 slideShow Window.dimensions (tickCount 4)