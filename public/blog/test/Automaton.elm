
import Automaton


input = lift2 (\a b -> (a,b)) Mouse.isDown Mouse.position

formsAutomaton =
    combine $ map dragForm
      [ filled cyan $ rect 50 50 (200,200)
      , sprite "yogi.jpg" 100 100 (100,400)
      , toForm (100,100) $ plainText "TEXT"
      ]

main = lift2 (uncurry collage) Window.dimensions
     $ run formsAutomaton input