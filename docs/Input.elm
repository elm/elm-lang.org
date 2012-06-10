
button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (245/255) (245/255) (245/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , width outer . box 2 $ plainText "&nbsp;" `above` body inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces px = List.map (\f -> f ()) . List.intersperse (\x -> height px $ plainText "") . List.map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

darkerGrey = rgb (114/255) (124/255) (129/255)
entry w (name, type, desc) =
  flow down . List.map (width w) $
    [ text . monospace . toText $ name ++ " :: " ++ type
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . List.map (width w) $ (text . bold $ toText name) : List.map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : List.map (group w) cats in
  lift (skeleton f) Window.width

----------------------

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

categories = [ ("Text Input", txt)
             , ("Check Boxes", boxes)
             , ("Drop Downs", drops)
             ]

main = createDocs "Input" categories