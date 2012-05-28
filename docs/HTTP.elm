
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
    [ text . monospace . toText $ name ++ (if List.length type == 0 then "" else " :: " ++ type)
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . List.map (width w) $ (text . bold $ toText name) : List.map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : List.map (group w) cats in
  lift (skeleton f) Window.width

----------------------

datatypes =
  [ ("data Response a = Waiting | Success a | Failure Int String", "", "The datatype for responses. Success contains only the returned message. Failures contain both an error code and an error message.")
  ]

singles =
  [ ("get", "String -> Signal (Response String)", "Performs an HTTP get request with the given address. Produces a signal that carries the response.")
  , ("post", "String -> Signal (Response String)", "Performs an HTTP post request with the given address. Produces a signal that carries the response.")
  ]

multis =
  [ ("gets", "Signal (Maybe String) -> Signal (Maybe (Response String))",
     "Produces a signal that carries 'Just' the response when given 'Just' a URL. Uses get requests.")
  , ("posts", "Signal (Maybe String) -> Signal (Maybe (Response String))",
     "Produces a signal that carries 'Just' the response when given 'Just' a URL. Uses post requests.")
  ]

categories =
  [ ("Responses", datatypes)
  , ("Single Requests", singles)
  , ("Multiple Requests", multis)
  ]

main = createDocs "HTTP" categories