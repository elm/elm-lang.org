
import Website.Docs (createDocs)


lifts =
  [ ("constant", "a -> Signal a", "Create a constant signal that never changes.")
  , ("lift", "(a -> b) -> Signal a -> Signal b", "Transform a signal with a given function.")
  , ("lift2", "(a -> b -> c) -> Signal a -> Signal b -> Signal c", "Combine two signals with a given function.")
  , ("lift3", "(a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d", "Combine three signals with a given function.")
  , ("lift4", "(a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e", "Combine four signals with a given function.")
  ]

folds =
  [ ("foldp", "(a -> b -> b) -> b -> Signal a -> Signal b", "Create a past-dependent signal. Each value given on the input signal will be accumulated, producing a new output value. For instance, (foldp (\t count -> count + 1) 0 (Time.every 3) counts up every time the timer ticks.")
  , ("count", "Signal a -> Signal Int", "Count the number of events that have occured.")
  ]

filters =
  [ ("keepIf", "(a -> Bool) -> a -> Signal a -> Signal a", "Keep only events that satisfy the given predicate. Elm does not allow undefined signals, so a base case must be provided in case the predicate is never satisfied.")
  , ("dropIf", "(a -> Bool) -> a -> Signal a -> Signal a", "Drop events that satisfy the given predicate. Elm does not allow undefined signals, so a base case must be provided in case the predicate is never satisfied.")
  , ("keepWhen", "Signal Bool -> a -> Signal a -> Signal a", "Keep events only when the first signal is true. When the first signal becomes true, the most recent value of the second signal will be propagated. Until the first signal becomes false again, all events will be propagated. Elm does not allow undefined signals, so a base case must be provided in case the first signal is never true.")
  , ("dropWhen", "Signal Bool -> a -> Signal a -> Signal a", "Drop events when the first signal is true. When the first signal becomes false, the most recent value of the second signal will be propagated. Until the first signal becomes true again, all events will be propagated. Elm does not allow undefined signals, so a base case must be provided in case the first signal is always true.")
  , ("dropRepeats", "Signal a -> Signal a", "Drop sequential repeated values. For example, if a signal produces the sequence [1,1,2,2,1], it becomes [1,2,1] by dropping the values that are the same as the previous value.")
  , ("sampleOn", "Signal a -> Signal b -> Signal b"
    , toText "Sample from the second input every time an event occurs on the first input. For example, " ++ monospace (toText "sampleOn clicks (every 1)") ++ toText " will give the approximate time of the latest click.")
  ]

categories = 
  [ ("Lifts (Transforming and Combining Signals)", lifts)
  , ("Folds (Past-Dependent Transformations)", folds)
  , ("Filters", filters)
  ]

main = createDocs "Signal" categories