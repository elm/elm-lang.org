
--  Three Shades of Grey

c1 = rgba 255 255 255 0.4
c2 = rgba 110 110 110 0.5
c3 = rgb  244 244 244


--  Clickable left and right arrows

arrow theta =
    let w = 30
        h = 60
        x = w * 2/7
        y = h*3/11
    in  collage (round w) (round h)
        [ filled c1 <| rect w h (w/2, h/2)
        , rotate theta . filled c2 <|
                 polygon [ (x,0),(-x,y),(-x,-y) ] (w/2,h/2)
        ]

(leftArrow , leftClicked ) = Mouse.isClickedOn (arrow 0.5)
(rightArrow, rightClicked) = Mouse.isClickedOn (arrow 0)


--  Helper functions

ith i lst = case lst of { x::xs -> if i == 0 then x else ith (i-1) xs }
safeIth i lst = ith (i `mod` length lst) lst

countTrue = count . keepIf id True
index = lift2 (-) (countTrue rightClicked) (countTrue leftClicked)


--  Actual light-box code

lightBox imgW imgH imgs =
  let disp = container (imgW `div` 2) imgH in
  let scene (w,h) index =
       color c3 . container w h middle <| layers
        [ fittedImage imgW imgH (safeIth index imgs)
        , disp midLeft leftArrow `beside` disp midRight rightArrow ]
  in  lift2 scene Window.dimensions index

images = [ "book.jpg", "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ]

main = lightBox 472 315 images
