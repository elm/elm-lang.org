
import Website.Docs (createDocs)

dims =
  [ ("dimensions", "Signal (Int,Int)", "The current dimensions of the window (i.e. the area viewable to the user, not including scroll bars).")
  , ("width", "Signal Int", "The current width of the window.")
  , ("height", "Signal Int", "The current height of the window.")
  ]

categories = [ ("Dimensions", dims) ]

main = createDocs "Window" categories