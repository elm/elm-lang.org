
import Website.Docs (createDocs)

ticker =
  [ ("every", "Time -> Signal Time", "Takes a time interval t. The resulting signal shows the time since the program began, updated every t seconds.")
  ]

other =
  [ ("before", "Time -> Signal Bool", "Takes a time interval t. The resulting signal is true until t seconds have passed, then false.")
  , ("after", "Time -> Signal Bool", "Takes a time interval t. The resulting signal is false until t seconds have passed, then true.")
  ]


categories = [ ("Tickers", ticker), ("Simple Timers", other) ]

main = createDocs "Time" categories