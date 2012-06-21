
import Website.Docs

txt =
  [ ("textField", "String -> (Element, Signal String)", "Create a standard text field and a signal that represents the current user input in that field. The input string is \"ghost-text\" that appears when the user has yet to input any text.")
  , ("password", "String -> (Element, Signal String)", "Create a standard password field and a signal representing its current content. The input string is \"ghost-text\" that appears when the user has yet to input any text.")
  , ("textArea", "Int -> Int -> (Element, Signal String)", "Create a larger area for text and a signal representing its content. The input integers are the width and height of the text area in characters. This is useful for longer text input.")
  ]

boxes =
  [ ("checkBox", "Bool -> (Element, Signal Bool)", "Create a check box and a signal representing whether the box is checked. The input specifies the default value for the box.") ]

drops =
  [ ("stringDropDown", "[String] -> (Element, Signal String)", "Input a list of options to create a drop down menu and a signal of the currently selected option.")
  , ("dropDown", "[(String,a)] -> (Element, Signal a)", "Input a list of options. Each option is a pair containing the text to be displayed and a corresponding value of any type. This creates a drop down menu and a signal of the value corresponding to the currently selected option.")
  ]

buttons =
  [ ("button", "String -> (Element, Signal Bool)", "Create a button and a signal representing whether the button has been pressed. The input specifies the string displayed on the button.") ]

categories = [ ("Text Input", txt)
             , ("Buttons", buttons)
             , ("Check Boxes", boxes)
             , ("Drop Downs", drops)
             ]

main = createDocs "Signal.Input" categories