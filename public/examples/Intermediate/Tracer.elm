-- by Joe Collard, http://people.cs.umass.edu/~jcollard/

import Automaton
import Graphics.Collage

-- parameters
areaSize = 400  -- size of area to draw in
size  = 3       -- size of "pixel"
rate  = 32      -- Refresh rate
trail = 100     -- How long
av  = 3         -- angular velocity of the inner circle
av' = 5         -- angular velocity of the outer circle
cx = areaSize/2 -- CenterX of the inner circle
cy = areaSize/2 -- CenterX of the inner circle
r  = areaSize/3 -- Radius of the inner circle
r' = r/3        -- Radius of the outer circle

-- Calculate the angle at the specified time
angle  t = degrees (t*av )
angle' t = degrees (t*av')

-- Calculate positions at the specified time
posX  t = cx + r * cos (3 * angle t)
posY  t = cy + r * sin (2 * angle t)
posX' t = posX t + r' * cos (angle' t)
posY' t = posY t + r' * sin (angle' t)
pos'  t = (posX' t, posY' t)

-- Draw a single pixel of a particular color and size at the specified time
pixel color time size = 
  let off = -areaSize/2
      p = pos' time
      move' (x, y) = move (x+off, y+off)
  in  move' p (filled color (circle size))
  
-- Oscillate the colors up and down
osc n = if n <= 255 then n else (255 - (n `mod` 255))
c m t = osc ((t*m) `mod` 510)
red   = c 3
green = c 5
blue  = c 7

-- Draws elements at the specified time
drawing time = 
  let times  = filter (\x -> x > 0) <| adjust time (\t -> t - 1) trail
      sizes  = adjust size (\s -> s - size/trail) trail
      reds   = map red times
      greens = map green times
      blues  = map blue times
      alphas = adjust 1 (\a -> a - 1/trail) trail
      colors = zipWith4 rgba reds greens blues alphas
      pixels = zipWith3 pixel colors times sizes
  in  collage areaSize areaSize pixels

-- Each time step, the time increases by one
theState = Automaton.state 0 (\a b -> b + 1)

-- Step every rate milliseconds
stateSignal = Automaton.run theState 0 <| every <| rate*millisecond

main = drawing <~ stateSignal



-- Helper functions

zipWith4 : (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f ws xs ys zs = 
  case (ws,xs,ys,zs) of
    (w::ws, x::xs, y::ys, z::zs) -> f w x y z :: zipWith4 f ws xs ys zs
    _ -> []

zipWith3 : (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f xs ys zs = 
  case (xs,ys,zs) of
    (x::xs, y::ys, z::zs) -> f x y z :: zipWith3 f xs ys zs
    _ -> []

-- Create a list that starts with x and adjusts n times
adjust x by n = case n of
  0 -> []
  _ -> x:: adjust (by x) by (n-1)
