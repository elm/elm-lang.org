map f xs = case xs of { x:xs -> f x : map f xs ; [] -> [] }
intersperse sep xs =
  case xs of { a:b:cs -> a : sep : intersperse sep (b:cs)
             ; a:[] -> [a] ; [] -> [] }
intercalate sep xs =
  case xs of { a:b:cs -> a ++ sep ++ intercalate sep (b:cs)
             ; a:[] -> a ; [] -> [] }

----------------------

button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . map button $
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

addSpaces px = map (\f -> f ()) . intersperse (\x -> height px $ plainText "") . map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

darkerGrey = rgb (114/255) (124/255) (129/255)
entry w (name, type, desc) =
  flow down . map (width w) $
    [ text . monospace . toText $ name ++ " :: " ++ type
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . map (width w) $ (text . bold $ toText name) : map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : map (group w) cats in
  lift (skeleton f) Window.width

----------------------

creation =
  [ ("toText", "String -> Text", "Convert a string into text which can be styled and displayed.")
  , ("link", "String -> Text -> Text", "Create a link.")
  ]

textSize =
  [ ("header", "Text -> Text", "Makes text big and noticable.")
  , ("height", "Number -> Text -> Text", "Sets the height of text in \"ems\". 1em is the normal height of text. 2ems is twice that height.")
  ]

styles =
  [ ("italic", "Text -> Text", "Italicize a string.")
  , ("bold", "Text -> Text", "Make a string bold.")
  , ("underline", "Text -> Text", "Underline a string.")
  , ("overline", "Text -> Text", "Draw a line above a string.")
  , ("strikeThrough", "Text -> Text", "Draw a line through a string.")
  , ("color", "Color -> Text -> Text", "Set the color of a string.")
  , ("monospace", "Text -> Text", "Switch to a monospace typeface. Good for code snippets.")
  ]

categories = 
  [ ("Creating Text", creation)
  , ("Styling Text", styles)
  , ("Ajust Text Size", textSize)
  ]

main = createDocs "Text" categories