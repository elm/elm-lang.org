import Keyboard
import Window

-- MODEL
-- Mario starts off as x=0,y=0 distance from his origan, with a velocity vx,vy of zero, and facing right
mario = { x=0, y=0, vx=0, vy=0, dir="right" }


-- UPDATE -- ("m" is for Mario)

jump {y} m =           -- jump needs 2 arguments. {y} is a keyboard field with a y value, m is a mario object
  if y > 0 && m.y == 0 -- if up arrow is pressed AND mario isn't jumping
  then { m | vy <- 5 } -- then mario's vertical velocity is equal to 5
  else m               -- otherwise no change

gravity t m =                    -- t is a time period, m is a mario
  if m.y > 0                     -- if mario is in the air
  then { m | vy <- m.vy - t/4 }  -- then mario's velocity is decreased by t/4
  else m                         -- otherwise nothing
  
walk {x} m = {                          -- {x} is a keyboard field with an x value, m is a mario object
  m | vx  <- toFloat x,                 -- mario's horizontal velocity is whatever value is returned by the leyboard field
      dir <- if | x < 0     -> "left"   -- change mario's direction to left if the keyboard.x is negative,
                | x > 0     -> "right"  -- change to right is positive
                | otherwise -> m.dir    -- leave unchanged (set it to itself) if keyboard is zero
  }

physics t m = {                  -- t is a time period, m is a mario
  m | x <- m.x + t*m.vx ,        -- marios x position is increased by t multiples of his current x velocity
      y <- max 0 (m.y + t*m.vy)  -- marios y position is increased by t multiples of his current y velocity, but can not go below zero
  }
  
-- step is called by foldp. foldp does what is called a partial call on step.
-- Physics, walk, gravity, and jump, all require a mario object to work with, yet there is no mario object.
-- The mario object is there implicitly, due to the way step is called natively by foldp
-- The mario object will also be called as the first argument to  physics, walk, havitty, and jump.
-- Since these actions all require a t or dir first, we call them partially outselves, using the t or dir argument.

-- the actions partial functions are compossted in reverse order. jump, gravity, walk, then physics
-- same as if "(physics t (walk dir (gravity t (jump dir mario))))" were called

step (t,dir) =
  physics t .
  walk dir .
  gravity t .
  jump dir


main = render                      -- our browser is the result of the render function.
        <~ Window.dimensions       -- render will be passed in a lifted value of
        ~ foldp step mario input   -- (dimensions mario). dimensions is itself a tuple (x,y)
        
        -- foldp works by iteratively creating a mario object.
        -- foldp a b c means... everytime "c" changes, call the function "a" with the parameters of "c" and whatever was last returned by "a"
        -- unless "a" has never been called before, in which case use "b" as the first "returned value"
        -- this is also known as "reduce" in other languages.
        
        {--
        
          imagine:
            a function "a", defined as a q r = 1 + q + r
            a value "b" defined as 3,
            a symbol "c" that triggers 5 times with the values [2,4,3,4,4]
        
          then:
            the result of foldp a b c would first be 1 + 3 + 2 = 6
            the next time "c" triggers, the result would be 1 + 6 + 4 = 11
            after all 5 "c"'s have triggered, the result would be 25
            
        --}


-- DISPLAY
render (w',h') mario =
  let w = toFloat w'
      h = toFloat h'
      verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src  = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w' h' [
    rect w h  |> filled (rgb 174 238 238),
    rect w 50 |> filled (rgb 74 163 41)
              |> move (0, 25-h/2),
    toForm (image 35 35 src) |> move (mario.x, mario.y + 63 - h/2)
    ]

-- MARIO

input = let trigger = (\t -> t/15) <~ fps 25
        in sampleOn( trigger )( (,) <~ trigger ~ Keyboard.arrows)


