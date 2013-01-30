
import Website.Docs (createDocs2)

create =
  [ ("pure", "(i -> o) -> Automaton i o",[markdown|Create an automaton with no memory. It just applies the given function to every input.|])
  , ("init", "o -> (i -> o -> o) -> Automaton i o"
    ,[markdown|Create a stateful automaton. It takes a step function that uses the input and previous output to compute each new output. It also needs a default value for the first step when nothing has been output yet.|])
  , ("init'","s -> (i -> s -> (o,s)) -> Automaton i o"
    ,[markdown|Create an automaton with hidden state. Requires an initial state and a step function to step the state forward and produce an output.|])
  ]

combine =
  [ ("(>>>)", "Automaton a b -> Automaton b c -> Automaton a c", [markdown|Compose two automatons, chaining them together.|])
  , ("(<<<)", "Automaton b c -> Automaton a b -> Automaton a c", [markdown|Compose two automatons, chaining them together.|])
  , ("combine","[Automaton a b] -> Automaton a [b]", [markdown|Combine a list of automatons into a single automaton that produces a list.|])
  ]

eval =
  [ ("run" , "Automaton a b -> Signal a -> Signal b", [markdown|Run an automaton on a given signal. The automaton takes in &lsquo;a&rsquo; values and returns &lsquo;b&rsquo; values. The automaton steps forward whenever the input signal updates.|])
  , ("step", "Automaton a b -> a -> (b, Automaton a b)", [markdown|Step an automaton forward once with a given input.|])
  ]

misc =
  [ ("count", "Automaton a Int", [markdown|Count the number of steps taken.|])
  , ("draggable", "Form -> Automaton (Bool,(Int,Int)) Form", [markdown|Create a draggable form that can be dynamically created and added to a scene.|])
  ]

combine2 =
  [ ("(^>>)", "(a -> b) -> Automaton b c -> Automaton a c", [markdown|
Turn a pure function into an automaton and compose it with an existing automaton.|])
  , ("(>>^)", "Automaton a b -> (b -> c) -> Automaton a c", [markdown||])
  , ("(<<^)", "Automaton b c -> (a -> b) -> Automaton a c", [markdown||])
  , ("(^<<)", "(b -> c) -> Automaton a b -> Automaton a c", [markdown||])
  ]


categories = 
  [ ("Create", create)
  , ("Evaluate", eval)
  , ("Combine", combine)
  , ("Pre-defined Automatons", misc)
  , ("Composition Helpers", combine2)
  ]

intro = [markdown|
This library is a way to package up dynamic behavior. It makes it easier to
dynamically create dynamic components. See the [original release
notes](/blog/announce/version-0.5.0.elm) on this library to get a feel for how
it can be used.
|]

main = createDocs2 "Automaton" intro categories