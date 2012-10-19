
import Website.Docs (createDocs)

create =
  [ ("pure", "(a -> b) -> Automaton a b","")
  , ("init", "b -> (a -> b -> b) -> Automaton a b","")
  , ("init'","s -> (a -> s -> (b,s)) -> Automaton a b","")
  ]

combine =
  [ ("(>>>)", "Automaton a b -> Automaton b c -> Automaton a c", "")
  , ("(<<<)", "Automaton b c -> Automaton a b -> Automaton a c", "")
  , ("combine","[Automaton a b] -> Automaton a [b]","")
  ]

eval =
  [ ("run" , "Automaton a b -> Signal a -> Signal b", "Run an automaton on a given signal. The automaton updates whenever the value of the signal updates.")
  , ("step", "Automaton a b -> a -> (b, Automaton a b)", "Step an automaton forward once with a given input.")
  ]

misc =
  [ ("count", "Automaton a Int", "")
  , ("draggable", "Form -> Automaton (Bool,(Int,Int)) Form", "")
  ]

categories = 
  [ ("Create", create)
  , ("Combine", combine)
  , ("Evaluate", eval)
  , ("Pre-defined Automatons", misc)
  ]

main = createDocs "Automaton" categories