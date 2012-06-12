
import Website.Docs (createDocs)

ranges =
  [ ("inRange", "Int -> Int -> Signal Int", "Given a range from low to high, this produces a random number between 'low' and 'high' inclusive. The value in the signal does not change after the page has loaded.")
  , ("randomize", "Int -> Int -> Signal a -> Signal Int", "Given a range from low to high and a signal of values, this produces a new signal that changes whenever the input signal changes. The new values are random number between 'low' and 'high' inclusive.")
  ]

categories = [ ("In a Range", ranges) ]

main = createDocs "Signal.Random" categories