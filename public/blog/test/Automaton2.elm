
import Automaton
import Signal.Input
import Signal.Random
import Data.List

(butnMore,pressMore) = button "  +  "
(butnLess,pressLess) = button "  -  "

data Command = Incr | Decr | Idnt

formsAutomaton =
  let fstep (cmd,pos,color,mouse) fs =
          let fs' = case cmd of
                    { Incr -> fs ++ [dragForm $ filled color (rect 40 40 pos)]
                    ; Decr -> if fs == [] then [] else tail fs
                    ; Idnt -> fs }
          in  unzip $ map (step mouse) fs'
  in  init' [] fstep

allInput =
  let { commands =
            let { step decr incr = if decr then Decr else
                                   if incr then Incr else Idnt }
            in  lift2 step pressLess pressMore
      ; pos = lift2 (\x y -> (x,y)) (randomize 0 400 commands)
                                    (randomize 0 400 commands)
      ; color = lift3 rgb (randomize 0 255 commands)
                          (randomize 0 255 commands)
                          (randomize 0 255 commands)
      ; mouse = lift2 (\a b -> (a,b)) Mouse.isDown Mouse.position }
  in  lift4 (\a b c d -> (a,b,c,d)) commands pos color mouse

controls = 
  container 400 50 middle $
  flow right [ butnLess, plainText "  Number of Squares   ",  butnMore ]

display fs = collage 400 400 (outlined black (rect 400 400 (200,200)) : fs)

main = lift (\forms -> display forms `above` controls)
     $ run formsAutomaton allInput