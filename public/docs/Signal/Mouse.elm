
import Website.Docs (createDocs)


position =
  [ ("position", "Signal (Int,Int)", "The current mouse position.")
  , ("x", "Signal Int", "The current x-coordinate of the mouse.")
  , ("y", "Signal Int", "The current y-coordinate of the mouse.")
  ]

mbuttons =
  [ ("isDown", "Signal Bool", "The current state of the left mouse-button. True when the button is down, and false otherwise.")
  , ("isClicked", "Signal Bool", "True immediately after the left mouse-button has been clicked, and false otherwise.")
  , ("clicks", "Signal ()", "Always equal to unit. Event triggers on every mouse click.")
  , ("isClickedOn", "Element -> (Element, Signal Bool)", "Determine whether an element has been clicked. The resulting pair is a signal of booleans that is true when its paired element has been clicked. The signal is True immediately after the left mouse-button has been clicked, and false otherwise.")
  ]

categories = [ ("Position", position), ("Button Status", mbuttons) ]

main = createDocs "Signal.Mouse" categories