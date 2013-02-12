
import Website.Docs (createDocs)

sigs =
  [ ("keysDown", "Signal [Int]", "The key codes of they keboard keys that are currently pressed, in the order that they were pressed. The key codes are equal to the JavaScript keyCode for each key.")
  , ("charPressed", "Signal (Maybe Int)", "The char code of the currently pressed key. When a key is pressed, its char code appears in this signal very briefly. The char codes are given by the JavaScript expression (e.charCode || e.keyCode)")
  ]

categories = [ ("Keyboard Status", sigs) ]

main = createDocs "Keyboard.Raw" categories
