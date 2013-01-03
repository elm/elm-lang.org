

ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]
slideShow (w,h) index =
  let i = index `mod` length images in
  color black . container w h middle . image 472 315 $ ith i images


clickCount = count Mouse.clicks
tickCount t = count (every t)

main = slideShow <~ Window.dimensions ~ tickCount (4 * second)

-- Be sure to also try this with `clickCount` which switches images
-- each time the user clicks the mouse. You can also try changing the
-- time interval (e.g. `tickCount 7`)
