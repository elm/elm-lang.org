
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


math =
  [ ("(+), (-), (*), (/)", "Number -> Number -> Number", "Functions for basic mathematics.")
  , ("sin, cos, tan, asin, acos, atan", "Number -> Number", "Basic functions of trigonometry.")
  , ("sqrt", "Number -> Number", "Take the square root of a number.")
  , ("rem", "Int -> Int -> Int", "Finds the remainder after dividing one number by another. (4 `rem` 3) == 1.")
  , ("mod", "Int -> Int -> Int", "Perform modular arithmetic. (-2 `mod` 5) == 3")
  , ("abs", "Number -> Number", "Take the absolute value of a number.")
  , ("logBase", "Number -> Number -> Number", "Calculate the logarithm of a number with a given base: logBase 10 100 == 2.")
  , ("min, max", "Number -> Number -> Number", "Given two numbers, returns the smaller (or greater respectively).")
  , ("clamp", "Number -> Number -> Number -> Number", "Clamps a number within a given range, so (clamp 100 200 x) is 200 for x >= 200, 100 for x <= 100, and x for any 100 < x < 200 ")
  ]

funcs =
  [ ("(.)", "(b -> c) -> (a -> b) -> (a -> c)", "Function composition: f . g == (\\x -> f (g x))")
  , ("($)", "(a -> b) -> a -> b", "Function application (f $ x == f x). This function is useful for avoiding parenthesis. Consider the following code to create a text element: (text (monospace (toText \"code\"))). This can also be written as (text . monospace $ toText \"code\").")
  , ("id", "a -> a", "Given a value, returns exactly the same value.")
  , ("flip", "(a -> b -> c) -> (b -> a -> c)", "Flips the order of the first two arguments to a function.")
  ]

categories = 
  [ ("Mathematics", math)
  , ("Function Helpers", funcs)
  ]

main = createDocs "Prelude" categories