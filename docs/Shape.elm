
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
  [ ("ngon", "Number -> Number -> (Number,Number) -> Shape", "Create regular polygons with n sides. This function takes the number of sides, the radius (distance from the center to a vertex), and a center.")
  , ("rect", "Number -> Number -> (Number,Number) -> Shape", "Create a rectangle with a given width, height, and center position (in that order).")
  , ("oval", "Number -> Number -> (Number,Number) -> Shape", "Create an oval with a given width, height, and center position (in that order).")
  , ("polygon", "[(Number,Number)] -> (Number,Number) -> Shape", "Create an arbitrary polygon from an list of vertexes and a center point. The vertexes should be listed relative to the origin (0,0). They are then translated to the given center.")
  ]

toForm =
  [ ("filled", "Color -> Shape -> Form", "Fill a shape with a given color.")
  , ("outlined", "Color -> Shape -> Form", "Outline a shape with a given color.")
  , ("customOutline", "[Number] -> Color -> Shape -> Form", "Outline a shape with a given pattern and color. The pattern is specified by the list of numbers. For instance, the pattern [8,4] creates long dashes (8 pixels long) followed by short spaces (4 pixels long).")
  ]

transform =
  [ ("move", "Number -> Number -> Shape -> Shape", "Translate a shape by a given x and y offset.")
  , ("rotate", "Number -> Number -> Shape -> Shape", "Rotate a shape by a given fraction of a full turn (not degrees or radians). For instance, (rotate (1/2) s) will rotate s by a half turn.")
  , ("scale", "Number -> Shape -> Shape", "Scale a shape by a given scale factor.")
  ]

categories = 
  [ ("Creating Shapes", creation)
  , ("Transforming Shapes", transform)
  , ("Turning Shapes into Forms", toForm)
  ]

main = createDocs "Shape" categories