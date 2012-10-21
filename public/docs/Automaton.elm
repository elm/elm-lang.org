
import Website.Docs (createDocs)

create =
  [ ("pure", "(i -> o) -> Automaton i o","Create an automaton with no memory. It just applies the given function to every input.")
  , ("init", "o -> (i -> o -> o) -> Automaton i o"
    ,"Create a stateful automaton. It takes a step function that uses the input and previous output to compute each new output. It also needs a default value for the first step when nothing has been output yet.")
  , ("init'","s -> (i -> s -> (o,s)) -> Automaton i o"
    ,"Create an automaton with hidden state. Requires an initial state and a step function to step the state forward and produce an output.")
  ]

combine =
  [ ("(>>>)", "Automaton a b -> Automaton b c -> Automaton a c", "Compose two automatons, chaining them together.")
  , ("(<<<)", "Automaton b c -> Automaton a b -> Automaton a c", "Compose two automatons, chaining them together.")
  , ("combine","[Automaton a b] -> Automaton a [b]", "Combine a list of automatons into a single automaton that produces a list.")
  ]

eval =
  [ ("run" , "Automaton a b -> Signal a -> Signal b", "Run an automaton on a given signal. The automaton takes in &lsquo;a&rsquo; values and returns &lsquo;b&rsquo; values. The automaton steps forward whenever the input signal updates.")
  , ("step", "Automaton a b -> a -> (b, Automaton a b)", "Step an automaton forward once with a given input.")
  ]

misc =
  [ ("count", "Automaton a Int", "Count the number of steps taken.")
  , ("draggable", "Form -> Automaton (Bool,(Int,Int)) Form", "Create a draggable form that can be dynamically created and added to a scene.")
  ]

categories = 
  [ ("Create", create)
  , ("Evaluate", eval)
  , ("Combine", combine)
  , ("Pre-defined Automatons", misc)
  ]

main = createDocs "Automaton" categories