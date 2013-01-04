
desired = 50
timestep = fps desired


-- Use time and keyboard input to determine the
-- position of the square.

addKey k (x,y) =
  if | k == 37   -> ( x-1 ,  y  )
     | k == 38   -> (  x  , y-1 )
     | k == 39   -> ( x+1 ,  y  )
     | k == 40   -> (  x  , y+1 )
     | otherwise -> (  x  ,  y  )

velocity = lift (foldl addKey (0,0)) Keyboard.Raw.keysDown

delta = let bothZero (x,y) = x == 0 && y == 0
            scale s (x,y) = (x*s/10, y*s/10)
        in  dropIf bothZero (0,0) $
            sampleOn timestep (lift2 scale timestep velocity)

position = let add (a,b) (c,d) = (a+c, b+d) in
           foldp add (0,0) delta


-- Display moving square and FPS on screen.
screen pos actual =
  flow down [ collage 400 400 [ outlined grey (rect 400 400 (200,200))
                              , outlined black (rect 40 40 pos) ]
            , plainText "Move the square around with the arrow keys."
            , plainText $ "Actual frames per second: " ++ show actual
            , plainText $ "Desired frames per second: " ++ show desired
            ]

averageFPS = truncate . (/) second <~ average 40 timestep

main = lift2 screen position averageFPS
