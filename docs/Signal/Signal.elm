
import Website.Docs (createDocs)


lifts =
  [ ("constant", "a -> Signal a", "Create a constant signal that never changes.")
  , ("lift", "(a -> b) -> Signal a -> Signal b", "Transform a signal with a given function.")
  , ("lift2", "(a -> b -> c) -> Signal a -> Signal b -> Signal c", "Combine two signals with a given function.")
  , ("lift3", "(a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d", "Combine three signals with a given function.")
  , ("lift4", "(a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e", "Combine four signals with a given function.")
  ]

folds =
  [ ("foldp", "(a -> b -> b) -> b -> Signal a -> Signal b", "Create a past-dependent signal. Each value given on the input signal will be accumulated, producing a new output value. For instance, (foldp (\t count -> count + 1) 0 (Time.every 3) counts up every time the timer ticks.") ]

categories = 
  [ ("Lifts (Transforming and Combining Signals)", lifts)
  , ("Folds (Past-Dependent Transformations)", folds)
  ]

main = createDocs "Signal" categories