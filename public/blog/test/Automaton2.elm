
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
  let { commands = let step less more = if less then Decr else
                                        if more then Incr else Idnt
                   in  lift2 step pressLess pressMore
      ; rand n = randomize 0 n commands
      ; pos = lift2 (,) (rand 400) (rand 400)
      ; color = lift3 rgb (rand 255) (rand 255) (rand 255)
      ; mouse = lift2 (,) Mouse.isDown Mouse.position }
  in  lift4 (,,,) commands pos color mouse

controls = 
  container 400 50 middle $
  flow right [ butnLess, plainText "  Number of Squares   ",  butnMore ]

display fs = collage 400 400 (outlined black (rect 400 400 (200,200)) : fs)

main = lift (\forms -> display forms `above` controls)
     $ run formsAutomaton allInput