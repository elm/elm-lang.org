
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


creation =
  [ ("line", "[(Number,Number)] -> Line", "Create an arbitrary line from an list of vertexes.")
  ]

toForm =
  [ ("solid", "Color -> Line -> Form", "Create a solid line with a given color.")
  , ("dashed", "Color -> Line -> Form", "Create a dashed line with a given color.")
  , ("dotted", "Color -> Line -> Form", "Create a dotted line with a given color.")
  , ("customLine", "[Number] -> Color -> Line -> Form", "Create a custom line with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  ]

transform =
  [ ("move", "Number -> Number -> Line -> Line", "Translate a shape by a given x and y offset.")
  , ("rotate", "Number -> Number -> Line -> Line", "Rotate a shape by a given fraction of a full turn (not degrees or radians). For instance, (rotate (1/2) s) will rotate s by a half turn.")
  , ("scale", "Number -> Line -> Line", "Scale a shape by a given scale factor.")
  ]

categories = 
  [ ("Creating Lines", creation)
  , ("Transforming Lines", transform)
  , ("Turning Lines into Forms", toForm)
  ]

main = createDocs "Line" categories